#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

static void resetStack(VM* vm) {
	vm->stackTop = vm->stack;
}

static void runtimeError(VM* vm, const char* format, ...) {
	va_list args;
	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	fputs("\n", stderr);

	size_t instruction = vm->ip - vm->chunk->code - 1;
	int line = vm->chunk->lines[instruction];
	fprintf(stderr, "[line %d] in script\n", line);
	resetStack(vm);
}

void initVM(VM* vm) {
	resetStack(vm);
	vm->objects = NULL;
	initTable(&vm->globals);
	initTable(&vm->strings);
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
#define READ_BYTE() (*vm->ip++)
#define READ_CONSTANT() (vm->chunk->constants.values[READ_BYTE()])
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
#define NUM_OP_ASSIGN(op) \
	do { \
		ObjString* name = READ_STRING(); \
		Value* value; \
		if (!tableGetPtr(&vm->globals, name, &value)) { \
			runtimeError(vm, "Undefined variable '%s'.", name->chars); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
		if (IS_CONSTANT(*value)) { \
			runtimeError(vm, "Global '%s' is constant.", name->chars); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
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

		disassembleInstruction(vm->chunk, (int)(vm->ip - vm->chunk->code));
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
			if (!tableAdd(&vm->globals, name, AS_CONSTANT(peek(vm, 0)))) {
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
			if (IS_CONSTANT(*value)) {
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
			if (IS_CONSTANT(*value)) {
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
		case OP_SUBTRACT_SET_GLOBAL:	NUM_OP_ASSIGN(-= ); break;
		case OP_MULTIPLY_SET_GLOBAL:	NUM_OP_ASSIGN(*= ); break;
		case OP_DIVIDE_SET_GLOBAL:		NUM_OP_ASSIGN(/= ); break;
		case OP_MODULUS_SET_GLOBAL: {
			ObjString* name = READ_STRING();
			Value* value;
			if (!tableGetPtr(&vm->globals, name, &value)) {
				runtimeError(vm, "Undefined variable '%s'.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			if (IS_CONSTANT(*value)) {
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
			push(vm, vm->stack[index]);
			break;
		}
		case OP_SET_LOCAL: {
			uint8_t index = READ_BYTE();
			vm->stack[index] = AS_VAR(pop(vm));
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
			push(vm, NUMBER_VAL(-AS_NUMBER(pop(vm))));
			break;
		case OP_NOT:
			push(vm, BOOL_VAL(isFalsey(pop(vm))));
			break;
		case OP_RETURN: {
			// exit interpreter
			return INTERPRET_OK;
		}
		}
	}

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
#undef NUM_OP_ASSIGN
}

InterpretResult interpret(VM* vm, const char* source) {
	Chunk chunk;
	initChunk(&chunk);

	if (!compile(vm, source, &chunk)) {
		freeChunk(&chunk);
		return INTERPRET_COMPILE_ERROR;
	}

	vm->chunk = &chunk;
	vm->ip = vm->chunk->code;

	InterpretResult result = run(vm);

	freeChunk(&chunk);
	return result;
}
