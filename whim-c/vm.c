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
	tableSet(vm, &vm->globals, AS_STRING(vm->stack[0]), vm->stack[1]);
	pop(vm);
	pop(vm);
}

static Value nativePrint(int argCount, Value* args) {
	for (int i = 0; i < argCount; i++) {
		printValue(args[i]);
	}
	printf("\n");
	return NIL_VAL;
}

static Value nativeTime(int argCount, Value* args) {
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

void initVM(VM* vm) {
	resetStack(vm);
	vm->objects = NULL;
	vm->bytesAllocated = 0;
	vm->nextGC = 1024 * 1024;

	vm->compiler = NULL;
	vm->grayCount = 0;
	vm->grayCapacity = 0;
	vm->grayStack = NULL;

	initTable(&vm->globals);
	initTable(&vm->strings);

	vm->initString = NULL;
	vm->typeString = NULL;
	vm->superString = NULL;
	vm->initString = copyString(vm, "init", 4);
	vm->typeString = copyString(vm, "type", 4);
	vm->superString = copyString(vm, "super", 5);

	defineNative(vm, "print", nativePrint);
	defineNative(vm, "time", nativeTime);
}

void freeVM(VM* vm) {
	freeTable(vm, &vm->globals);
	freeTable(vm, &vm->strings);

	vm->initString = NULL;
	vm->typeString = NULL;
	vm->superString = NULL;

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

static bool call(VM* vm, ObjClosure* closure, int argCount, bool popOne) {
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
	frame->slots = vm->stackTop - argCount;
	frame->popOne = popOne;
	return true;
}

static bool callValue(VM* vm, Value callee, int argCount) {
	if (IS_OBJ(callee)) {
		switch (OBJ_TYPE(callee)) {
		case OBJ_BOUND_METHOD: {
			ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
			vm->stackTop[-argCount - 1] = bound->receiver;
			return call(vm, bound->method, argCount + 1, false);
		}
		case OBJ_CLASS: {
			ObjClass* _class = AS_CLASS(callee);
			vm->stackTop[-argCount - 1] = OBJ_VAL(newInstance(vm, _class));

			Value initializer;
			while (_class != NULL) {
				if (tableGet(&_class->fields, vm->initString, &initializer)) {
					return call(vm, AS_CLOSURE(initializer), argCount + 1, false);
				}
				_class = _class->super;
			}

			if (argCount != 0) {
				runtimeError(vm, "Expected 0 arguments but got %d.", argCount);
				return false;
			}
			return true;
		}
		case OBJ_CLOSURE: return call(vm, AS_CLOSURE(callee), argCount, true);
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

static bool invokeFromClass(VM* vm, ObjClass* _class, ObjString* name, int argCount) {
	Value method;

	ObjClass* current = _class;
	while (current != NULL) {
		if (tableGet(&current->fields, name, &method)) {
			return call(vm, AS_CLOSURE(method), argCount, false);
		}
		current = current->super;
	}

	runtimeError(vm, "Undefined property '%s'.", name->chars);
	return false;
}

static bool invoke(VM* vm, ObjString* name, int argCount) {
	Value receiver = peek(vm, argCount);

	if (IS_INSTANCE(receiver)) {
		ObjInstance* instance = AS_INSTANCE(receiver);

		Value value;
		if (tableGet(&instance->fields, name, &value)) {
			vm->stackTop[-argCount - 1] = value;
			return callValue(vm, value, argCount);
		}

		return invokeFromClass(vm, instance->type, name, argCount + 1);
	}
	else if (IS_CLASS(receiver)) {
		ObjClass* _class = AS_CLASS(receiver);

		Value value;
		while (_class != NULL) {
			if (tableGet(&_class->fields, name, &value)) {
				vm->stackTop[-argCount - 1] = value;
				return callValue(vm, value, argCount);
			}
			_class = _class->super;
		}

		runtimeError(vm, "Undefined property '%s'.", name->chars);
		return false;
	}
	runtimeError(vm, "Only classes and instances have properties.");
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
	ObjString* b = AS_STRING(peek(vm, 0));
	ObjString* a = AS_STRING(peek(vm, 1));

	int length = a->length + b->length;
	char* chars = ALLOCATE(char, length + 1);
	memcpy(chars, a->chars, a->length);
	memcpy(chars + a->length, b->chars, b->length);
	chars[length] = '\0';

	ObjString* result = takeString(vm, chars, length);

	pop(vm);
	pop(vm);
	push(vm, OBJ_VAL(result));
}

static bool defineProperty(VM* vm, ObjString* name, Value obj, Value value) {
	Table* fields;
	if (IS_INSTANCE(obj)) {
		ObjInstance* instance = AS_INSTANCE(obj);
		fields = &instance->fields;

		if (name == vm->typeString) {
			if (IS_CLASS(value)) {
				instance->type = AS_CLASS(value);
			}
			else {
				runtimeError(vm, "Instance type must be a class.");
				return false;
			}
		}
	}
	else if (IS_CLASS(obj)) {
		ObjClass* _class = AS_CLASS(obj);
		fields = &_class->fields;

		if (name == vm->superString) {
			if (IS_CLASS(value)) {
				ObjClass* super = AS_CLASS(value);
				if (_class != super) {
					_class->super = super;
				}
				else {
					runtimeError(vm, "Class cannot be its own superclass.");
					return false;
				}
			}
			else {
				runtimeError(vm, "Superclass must be a class.");
				return false;
			}
		}
	}
	else {
		runtimeError(vm, "Only classes and instances have properties.");
		return false;
	}

	if (!tableAdd(vm, fields, name, value)) {
		runtimeError(vm, "Property '%s' already exists.", name->chars);
		return false;
	}

	return true;
}

static bool getProperty(VM* vm, ObjString* name, Value obj, bool doPop) {
	ObjClass* _class = NULL;
	bool bind = false;
	if (IS_INSTANCE(obj)) {
		ObjInstance* instance = AS_INSTANCE(obj);

		if (name == vm->typeString) {
			if (doPop) pop(vm);
			push(vm, OBJ_VAL(instance->type));
			return true;
		}

		Value value;
		if (tableGet(&instance->fields, name, &value)) {
			if (doPop) pop(vm);
			push(vm, value);
			return true;
		}
		_class = instance->type;
		bind = true;
	}
	else if (IS_CLASS(obj)) {
		_class = AS_CLASS(obj);
	}
	else {
		runtimeError(vm, "Only classes and instances have properties.");
		return false;
	}

	while (_class != NULL) {
		if (name == vm->superString && _class->super != NULL) {
			if (doPop) pop(vm);
			push(vm, OBJ_VAL(_class->super));
			return true;
		}

		Value value;
		if (tableGet(&_class->fields, name, &value)) {
			if (bind && IS_CLOSURE(value)) {
				// bind method
				ObjBoundMethod* bound = newBoundMethod(vm, obj, AS_CLOSURE(value));
				if (doPop) pop(vm);
				push(vm, OBJ_VAL(bound));
				return true;
			}
			else {
				if (doPop) pop(vm);
				push(vm, value);
				return true;
			}
		}
		_class = _class->super;
	}

	runtimeError(vm, "Undefined property '%s'.", name->chars);
	return false;
}

static bool setProperty(VM* vm, ObjString* name, Value obj, Value value) {
	Value* current = NULL;
	ObjClass* _class = NULL;
	if (IS_INSTANCE(obj)) {
		ObjInstance* instance = AS_INSTANCE(obj);
		if (tableGetPtr(&instance->fields, name, &current)) {
			if (name == vm->typeString) {
				if (IS_CLASS(value)) {
					instance->type = AS_CLASS(value);
				}
				else {
					runtimeError(vm, "Instance type must be a class.");
					return false;
				}
			}
		}
		_class = instance->type;
	}
	else if (IS_CLASS(obj)) {
		_class = AS_CLASS(obj);
	}

	while (current == NULL && _class != NULL) {
		if (tableGetPtr(&_class->fields, name, &current)) {
			if (name == vm->superString) {
				if (IS_CLASS(value)) {
					ObjClass* super = AS_CLASS(value);
					if (_class != super) {
						_class->super = super;
					}
					else {
						runtimeError(vm, "Class cannot be its own superclass.");
						return false;
					}
				}
				else {
					runtimeError(vm, "Superclass must be a class.");
					return false;
				}
			}
		}
		_class = _class->super;
	}

	if (current == NULL) {
		runtimeError(vm, "Undefined property '%s'.", name->chars);
		return false;
	}
	if (IS_CONST(*current)) {
		runtimeError(vm, "Property '%s' is constant.", name->chars);
		return false;
	}

	*current = value;
	return true;
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
		case OP_NIL:	push(vm, NIL_VAL); break;
		case OP_TRUE:	push(vm, BOOL_VAL(true)); break;
		case OP_FALSE:	push(vm, BOOL_VAL(false)); break;
		case OP_POP:	pop(vm); break;
		case OP_DEFINE_GLOBAL_CONST: {
			ObjString* name = READ_STRING();
			if (!tableAdd(vm, &vm->globals, name, AS_CONST(peek(vm, 0)))) {
				runtimeError(vm, "Global '%s' already exists.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			pop(vm);
			break;
		}
		case OP_DEFINE_GLOBAL_VAR: {
			ObjString* name = READ_STRING();
			if (!tableAdd(vm, &vm->globals, name, AS_VAR(peek(vm, 0)))) {
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
		case OP_DEFINE_PROPERTY_CONST:
		case OP_DEFINE_PROPERTY_CONST_POP: {
			ObjString* name = READ_STRING();
			if (!defineProperty(vm, name, peek(vm, 1), AS_CONST(peek(vm, 0)))) {
				return INTERPRET_RUNTIME_ERROR;
			}
			pop(vm);
			if (instruction == OP_DEFINE_PROPERTY_CONST_POP) pop(vm);
			break;
		}
		case OP_DEFINE_PROPERTY_VAR:
		case OP_DEFINE_PROPERTY_VAR_POP: {
			ObjString* name = READ_STRING();
			if (!defineProperty(vm, name, peek(vm, 1), AS_VAR(peek(vm, 0)))) {
				return INTERPRET_RUNTIME_ERROR;
			}
			pop(vm);
			if (instruction == OP_DEFINE_PROPERTY_VAR_POP) pop(vm);
			break;
		}
		case OP_GET_PROPERTY:
		case OP_GET_PROPERTY_POP: {
			ObjString* name = READ_STRING();
			if (!getProperty(vm, name, peek(vm, 0), instruction == OP_GET_PROPERTY_POP)) {
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_SET_PROPERTY: {
			ObjString* name = READ_STRING();
			if (!setProperty(vm, name, peek(vm, 1), AS_VAR(peek(vm, 0)))) {
				return INTERPRET_RUNTIME_ERROR;
			}
			pop(vm);
			pop(vm);
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
		case OP_INVOKE: {
			ObjString* name = READ_STRING();
			int argCount = READ_BYTE();
			if (!invoke(vm, name, argCount)) {
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
			Value* newTop = frame->slots;
			if (frame->popOne) newTop--;

			Value result = pop(vm);
			closeUpvalues(vm, newTop);
			vm->frameCount--;
			if (vm->frameCount == 0) {
				pop(vm);
				return INTERPRET_OK;
			}

			vm->stackTop = newTop;
			push(vm, result);
			frame = &vm->frames[vm->frameCount - 1];
			break;
		}
		case OP_CLASS: push(vm, OBJ_VAL(newClass(vm, READ_STRING()))); break;
		case OP_ANON_CLASS: push(vm, OBJ_VAL(newClass(vm, NULL))); break;
		}
	}

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(VM* vm, const char* source) {
	ObjFunction* function = compile(vm, source);
	if (function == NULL) return INTERPRET_COMPILE_ERROR;

	push(vm, OBJ_VAL(function));
	ObjClosure* closure = newClosure(vm, function);
	pop(vm);
	push(vm, OBJ_VAL(closure));
	call(vm, closure, 0, true);

	return run(vm);
}
