#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

static void resetStack(VM* vm) {
	vm->stackTop = vm->stack;
	vm->frameCount = 0;
	vm->openUpvalues = NULL;
}

static void runtimeError(VM* vm, const char* format, ...) {
	va_list args;
	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	fputs("\n", stderr);

	for (int i = vm->frameCount - 1; i >= 0; i--) {
		CallFrame* frame = &vm->frames[i];
		ObjFunction* function = frame->closure->function;
		size_t instruction = frame->ip - function->chunk.code - 1;
		fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
		if (function->name == NULL) {
			if (i == 0) {
				fprintf(stderr, "script\n");
			}
			else {
				fprintf(stderr, "fn()\n");
			}
		}
		else {
			fprintf(stderr, "%s()\n", function->name->chars);
		}
	}

	resetStack(vm);
}

static void defineNative(VM* vm, const char* name, NativeFn function) {
	push(vm, OBJ_VAL(copyString(vm, name, (int)strlen(name))));
	push(vm, OBJ_VAL(newNative(vm, function)));
	tableSet(&vm->globals, AS_STRING(vm->stack[0]), vm->stack[1]);
	pop(vm);
	pop(vm);
}

static Value nativePrint(int argCount, Value* args) {
	for (int i = 0; i < argCount; i++) {
		printValue(args[i]);
	}
	return NIL_VAL;
}

static Value nativeTime(int argCount, Value* args) {
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

void initVM(VM* vm) {
	resetStack(vm);
	vm->objects = NULL;
	initTable(&vm->globals);
	initTable(&vm->strings);

	defineNative(vm, "print", nativePrint);
	defineNative(vm, "time", nativeTime);
}

void freeVM(VM* vm) {
	freeTable(&vm->globals);
	freeTable(&vm->strings);
	freeObjects(vm);
}

void push(VM* vm, Value value) {
	*vm->stackTop = value;
	vm->stackTop++;
}

Value pop(VM* vm) {
	vm->stackTop--;
	return *vm->stackTop;
}

static Value peek(VM* vm, int distance) {
	return vm->stackTop[-1 - distance];
}

static bool call(VM* vm, ObjClosure* closure, int argCount) {
	if (argCount != closure->function->arity) {
		runtimeError(vm, "Expected %d arguments but got %d.", closure->function->arity, argCount);
		return false;
	}

	if (vm->frameCount == FRAMES_MAX) {
		runtimeError(vm, "Stack overflow.");
		return false;
	}

	CallFrame* frame = &vm->frames[vm->frameCount++];
	frame->closure = closure;
	frame->ip = closure->function->chunk.code;
	frame->slots = vm->stackTop - argCount - 1;
	return true;
}

static bool callValue(VM* vm, Value callee, int argCount) {
	if (IS_OBJ(callee)) {
		switch (OBJ_TYPE(callee)) {
		case OBJ_CLOSURE: return call(vm, AS_CLOSURE(callee), argCount);
		case OBJ_NATIVE: {
			NativeFn native = AS_NATIVE(callee);
			Value result = native(argCount, vm->stackTop - argCount);
			vm->stackTop -= argCount + 1;
			push(vm, result);
			return true;
		}
		}
	}
	runtimeError(vm, "Can only call functions and classes.");
	return false;
}

static ObjUpvalue* captureUpvalue(VM* vm, Value* local) {
	ObjUpvalue* prevUpvalue = NULL;
	ObjUpvalue* upvalue = vm->openUpvalues;
	while (upvalue != NULL && upvalue->location > local) {
		prevUpvalue = upvalue;
		upvalue = upvalue->next;
	}

	if (upvalue != NULL && upvalue->location == local) {
		return upvalue;
	}

	ObjUpvalue* createdUpvalue = newUpvalue(vm, local);
	createdUpvalue->next = upvalue;

	if (prevUpvalue == NULL) {
		vm->openUpvalues = createdUpvalue;
	}
	else {
		prevUpvalue->next = createdUpvalue;
	}

	return createdUpvalue;
}

static void closeUpvalues(VM* vm, Value* last) {
	while (vm->openUpvalues != NULL && vm->openUpvalues->location >= last) {
		ObjUpvalue* upvalue = vm->openUpvalues;
		upvalue->closed = *upvalue->location;
		upvalue->location = &upvalue->closed;
		vm->openUpvalues = upvalue->next;
	}
}

static bool isFalsey(Value value) {
	return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate(VM* vm) {
	ObjString* b = AS_STRING(pop(vm));
	ObjString* a = AS_STRING(pop(vm));

	int length = a->length + b->length;
	char* chars = ALLOCATE(char, length + 1);
	memcpy(chars, a->chars, a->length);
	memcpy(chars + a->length, b->chars, b->length);
	chars[length] = '\0';

	ObjString* result = takeString(vm, chars, length);
	push(vm, OBJ_VAL(result));
}

static ObjString* concatValue(VM* vm, ObjString* a) {
	ObjString* b = AS_STRING(pop(vm));

	int length = a->length + b->length;
	char* chars = ALLOCATE(char, length + 1);
	memcpy(chars, a->chars, a->length);
	memcpy(chars + a->length, b->chars, b->length);
	chars[length] = '\0';

	ObjString* result = takeString(vm, chars, length);
	return result;
}

static InterpretResult run(VM* vm) {
	CallFrame* frame = &vm->frames[vm->frameCount - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() \
	(frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op) \
	do { \
		if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) { \
			runtimeError(vm, "Operands must be numbers."); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
		double b = AS_NUMBER(pop(vm)); \
		double a = AS_NUMBER(pop(vm)); \
		push(vm, valueType(a op b)); \
	} while (false)
#define GLOBAL_NUM_OP_ASSIGN(op) \
	do { \
		ObjString* name = READ_STRING(); \
		Value* value; \
		if (!tableGetPtr(&vm->globals, name, &value)) { \
			runtimeError(vm, "Undefined variable '%s'.", name->chars); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
		if (IS_CONST(*value)) { \
			runtimeError(vm, "Global '%s' is constant.", name->chars); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
		if (!IS_NUMBER(*value) || !IS_NUMBER(peek(vm, 0))) { \
			runtimeError(vm, "Operands must be numbers."); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
		AS_NUMBER(*value) op AS_NUMBER(pop(vm)); \
	} while (false)
#define LOCAL_NUM_OP_ASSIGN(op) \
	do { \
		uint8_t index = READ_BYTE(); \
		Value* value = &frame->slots[index]; \
		if (!IS_NUMBER(*value) || !IS_NUMBER(peek(vm, 0))) { \
			runtimeError(vm, "Operands must be numbers."); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
		AS_NUMBER(*value) op AS_NUMBER(pop(vm)); \
	} while (false)
#define UPVALUE_NUM_OP_ASSIGN(op) \
	do { \
		uint8_t index = READ_BYTE(); \
		Value* value = frame->closure->upvalues[index]->location; \
		if (!IS_NUMBER(*value) || !IS_NUMBER(peek(vm, 0))) { \
			runtimeError(vm, "Operands must be numbers."); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
		AS_NUMBER(*value) op AS_NUMBER(pop(vm)); \
	} while (false)

	for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
		printf("          ");
		for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
			printf("[ ");
			printValue(*slot);
			printf(" ]");
		}
		printf("\n");

		disassembleInstruction(&frame->closure->function->chunk,
			(int)(frame->ip - frame->closure->function->chunk.code));
#endif

		uint8_t instruction;
		switch (instruction = READ_BYTE())
		{
		case OP_CONSTANT: {
			Value constant = READ_CONSTANT();
			push(vm, constant);
			break;
		}
		case OP_NIL: push(vm, NIL_VAL); break;
		case OP_TRUE: push(vm, BOOL_VAL(true)); break;
		case OP_FALSE: push(vm, BOOL_VAL(false)); break;
		case OP_POP: pop(vm); break;
		case OP_DEFINE_GLOBAL_CONST: {
			ObjString* name = READ_STRING();
			if (!tableAdd(&vm->globals, name, AS_CONST(peek(vm, 0)))) {
				runtimeError(vm, "Global '%s' already exists.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			pop(vm);
			break;
		}
		case OP_DEFINE_GLOBAL_VAR: {
			ObjString* name = READ_STRING();
			if (!tableAdd(&vm->globals, name, AS_VAR(peek(vm, 0)))) {
				runtimeError(vm, "Global '%s' already exists.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			pop(vm);
			break;
		}
		case OP_GET_GLOBAL: {
			ObjString* name = READ_STRING();
			Value value;
			if (!tableGet(&vm->globals, name, &value)) {
				runtimeError(vm, "Undefined variable '%s'.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			push(vm, value);
			break;
		}
		case OP_SET_GLOBAL: {
			ObjString* name = READ_STRING();
			Value* value;
			if (!tableGetPtr(&vm->globals, name, &value)) {
				runtimeError(vm, "Undefined variable '%s'.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			if (IS_CONST(*value)) {
				runtimeError(vm, "Global '%s' is constant.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			*value = AS_VAR(pop(vm));
			break;
		}
		case OP_ADD_SET_GLOBAL: {
			ObjString* name = READ_STRING();
			Value* value;
			if (!tableGetPtr(&vm->globals, name, &value)) {
				runtimeError(vm, "Undefined variable '%s'.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			if (IS_CONST(*value)) {
				runtimeError(vm, "Global '%s' is constant.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			if (IS_NUMBER(*value) && IS_NUMBER(peek(vm, 0))) {
				AS_NUMBER(*value) += AS_NUMBER(pop(vm));
			}
			else if (IS_STRING(*value) && IS_STRING(peek(vm, 0))) {
				AS_STRING(*value) = concatValue(vm, AS_STRING(*value));
			}
			else {
				runtimeError(vm, "Operands must both be numbers or strings.");
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_SUBTRACT_SET_GLOBAL:	GLOBAL_NUM_OP_ASSIGN(-= ); break;
		case OP_MULTIPLY_SET_GLOBAL:	GLOBAL_NUM_OP_ASSIGN(*= ); break;
		case OP_DIVIDE_SET_GLOBAL:		GLOBAL_NUM_OP_ASSIGN(/= ); break;
		case OP_MODULUS_SET_GLOBAL: {
			ObjString* name = READ_STRING();
			Value* value;
			if (!tableGetPtr(&vm->globals, name, &value)) {
				runtimeError(vm, "Undefined variable '%s'.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			if (IS_CONST(*value)) {
				runtimeError(vm, "Global '%s' is constant.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			if (!IS_NUMBER(*value) || !IS_NUMBER(peek(vm, 0))) {
				runtimeError(vm, "Operands must be numbers.");
				return INTERPRET_RUNTIME_ERROR;
			}
			AS_NUMBER(*value) = (double)((int64_t)AS_NUMBER(*value) % (int64_t)AS_NUMBER(pop(vm)));
			break;
		}
		case OP_GET_LOCAL: {
			uint8_t index = READ_BYTE();
			push(vm, frame->slots[index]);
			break;
		}
		case OP_SET_LOCAL: {
			uint8_t index = READ_BYTE();
			frame->slots[index] = pop(vm);
			break;
		}
		case OP_ADD_SET_LOCAL: {
			uint8_t index = READ_BYTE();
			Value* value = &frame->slots[index];
			if (IS_NUMBER(*value) && IS_NUMBER(peek(vm, 0))) {
				AS_NUMBER(*value) += AS_NUMBER(pop(vm));
			}
			else if (IS_STRING(*value) && IS_STRING(peek(vm, 0))) {
				AS_STRING(*value) = concatValue(vm, AS_STRING(*value));
			}
			else {
				runtimeError(vm, "Operands must both be numbers or strings.");
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_SUBTRACT_SET_LOCAL: LOCAL_NUM_OP_ASSIGN(-= ); break;
		case OP_MULTIPLY_SET_LOCAL: LOCAL_NUM_OP_ASSIGN(*= ); break;
		case OP_DIVIDE_SET_LOCAL:	LOCAL_NUM_OP_ASSIGN(/= ); break;
		case OP_MODULUS_SET_LOCAL: {
			uint8_t index = READ_BYTE();
			Value* value = &frame->slots[index];
			if (!IS_NUMBER(*value) || !IS_NUMBER(peek(vm, 0))) {
				runtimeError(vm, "Operands must be numbers.");
				return INTERPRET_RUNTIME_ERROR;
			}
			AS_NUMBER(*value) = (double)((int64_t)AS_NUMBER(*value) % (int64_t)AS_NUMBER(pop(vm)));
			break;
		}
		case OP_GET_UPVALUE: {
			uint8_t index = READ_BYTE();
			push(vm, *frame->closure->upvalues[index]->location);
			break;
		}
		case OP_SET_UPVALUE: {
			uint8_t index = READ_BYTE();
			*frame->closure->upvalues[index]->location = pop(vm);
			break;
		}
		case OP_ADD_SET_UPVALUE: {
			uint8_t index = READ_BYTE();
			Value* value = frame->closure->upvalues[index]->location;
			if (IS_NUMBER(*value) && IS_NUMBER(peek(vm, 0))) {
				AS_NUMBER(*value) += AS_NUMBER(pop(vm));
			}
			else if (IS_STRING(*value) && IS_STRING(peek(vm, 0))) {
				AS_STRING(*value) = concatValue(vm, AS_STRING(*value));
			}
			else {
				runtimeError(vm, "Operands must both be numbers or strings.");
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_SUBTRACT_SET_UPVALUE:	UPVALUE_NUM_OP_ASSIGN(-= ); break;
		case OP_MULTIPLY_SET_UPVALUE:	UPVALUE_NUM_OP_ASSIGN(*= ); break;
		case OP_DIVIDE_SET_UPVALUE:		UPVALUE_NUM_OP_ASSIGN(/= ); break;
		case OP_MODULUS_SET_UPVALUE: {
			uint8_t index = READ_BYTE();
			Value* value = frame->closure->upvalues[index]->location;
			if (!IS_NUMBER(*value) || !IS_NUMBER(peek(vm, 0))) {
				runtimeError(vm, "Operands must be numbers.");
				return INTERPRET_RUNTIME_ERROR;
			}
			AS_NUMBER(*value) = (double)((int64_t)AS_NUMBER(*value) % (int64_t)AS_NUMBER(pop(vm)));
			break;
		}
		case OP_EQUAL: {
			Value b = pop(vm);
			Value a = pop(vm);
			push(vm, BOOL_VAL(valuesEqual(a, b)));
			break;
		}
		case OP_NOT_EQUAL: {
			Value b = pop(vm);
			Value a = pop(vm);
			push(vm, BOOL_VAL(!valuesEqual(a, b)));
			break;
		}
		case OP_GREATER:		BINARY_OP(BOOL_VAL, > ); break;
		case OP_GREATER_EQUAL:	BINARY_OP(BOOL_VAL, >= ); break;
		case OP_LESS:			BINARY_OP(BOOL_VAL, < ); break;
		case OP_LESS_EQUAL:		BINARY_OP(BOOL_VAL, <= ); break;
		case OP_ADD: {
			if (IS_NUMBER(peek(vm, 0)) && IS_NUMBER(peek(vm, 1))) {
				double b = AS_NUMBER(pop(vm));
				double a = AS_NUMBER(pop(vm));
				push(vm, NUMBER_VAL(a + b));
			}
			else if (IS_STRING(peek(vm, 0)) && IS_STRING(peek(vm, 1))) {
				concatenate(vm);
			}
			else {
				runtimeError(vm, "Operands must both be numbers or strings.");
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_SUBTRACT:		BINARY_OP(NUMBER_VAL, -); break;
		case OP_MULTIPLY:		BINARY_OP(NUMBER_VAL, *); break;
		case OP_DIVIDE:			BINARY_OP(NUMBER_VAL, / ); break;
		case OP_MODULUS: {
			if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) {
				runtimeError(vm, "Operands must be numbers.");
				return INTERPRET_RUNTIME_ERROR;
			}
			int64_t b = (int64_t)AS_NUMBER(pop(vm));
			int64_t a = (int64_t)AS_NUMBER(pop(vm));
			push(vm, NUMBER_VAL((double)(a % b)));
			break;
		}
		case OP_NEGATE:
			if (!IS_NUMBER(peek(vm, 0))) {
				runtimeError(vm, "Operand must be a number.");
				return INTERPRET_RUNTIME_ERROR;
			}
			vm->stackTop[-1] = NUMBER_VAL(-AS_NUMBER(vm->stackTop[-1]));
			break;
		case OP_NOT:
			vm->stackTop[-1] = BOOL_VAL(isFalsey(vm->stackTop[-1]));
			break;
		case OP_JUMP: {
			uint16_t offset = READ_SHORT();
			frame->ip += offset;
			break;
		}
		case OP_JUMP_BACK: {
			uint16_t offset = READ_SHORT();
			frame->ip -= offset;
			break;
		}
		case OP_JUMP_IF_TRUE: {
			uint16_t offset = READ_SHORT();
			if (!isFalsey(peek(vm, 0))) frame->ip += offset;
			break;
		}
		case OP_JUMP_IF_FALSE: {
			uint16_t offset = READ_SHORT();
			if (isFalsey(peek(vm, 0))) frame->ip += offset;
			break;
		}
		case OP_JUMP_IF_TRUE_POP: {
			uint16_t offset = READ_SHORT();
			if (!isFalsey(pop(vm))) frame->ip += offset;
			break;
		}
		case OP_JUMP_IF_FALSE_POP: {
			uint16_t offset = READ_SHORT();
			if (isFalsey(pop(vm))) frame->ip += offset;
			break;
		}
		case OP_CALL: {
			int argCount = READ_BYTE();
			if (!callValue(vm, peek(vm, argCount), argCount)) {
				return INTERPRET_RUNTIME_ERROR;
			}
			frame = &vm->frames[vm->frameCount - 1];
			break;
		}
		case OP_CLOSURE: {
			ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
			ObjClosure* closure = newClosure(vm, function);
			push(vm, OBJ_VAL(closure));
			for (int i = 0; i < closure->upvalueCount; i++) {
				uint8_t isLocal = READ_BYTE();
				uint8_t index = READ_BYTE();
				if (isLocal) {
					closure->upvalues[i] = captureUpvalue(vm, frame->slots + index);
				}
				else {
					closure->upvalues[i] = frame->closure->upvalues[index];
				}
			}
			break;
		}
		case OP_CLOSE_UPVALUE:
			closeUpvalues(vm, vm->stackTop - 1);
			pop(vm);
			break;
		case OP_RETURN: {
			Value result = pop(vm);
			closeUpvalues(vm, frame->slots);
			vm->frameCount--;
			if (vm->frameCount == 0) {
				pop(vm);
				return INTERPRET_OK;
			}

			vm->stackTop = frame->slots;
			push(vm, result);
			frame = &vm->frames[vm->frameCount - 1];
			break;
		}
		}
	}

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
#undef GLOBAL_NUM_OP_ASSIGN
#undef LOCAL_NUM_OP_ASSIGN
#undef UPVALUE_NUM_OP_ASSIGN
}

InterpretResult interpret(VM* vm, const char* source) {
	ObjFunction* function = compile(vm, source);
	if (function == NULL) return INTERPRET_COMPILE_ERROR;

	push(vm, OBJ_VAL(function));
	ObjClosure* closure = newClosure(vm, function);
	pop(vm);
	push(vm, OBJ_VAL(closure));
	call(vm, closure, 0);

	return run(vm);
}
