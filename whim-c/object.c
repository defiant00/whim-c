#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
	(type*)allocateObject(vm, sizeof(type), objectType)

static Obj* allocateObject(VM* vm, size_t size, ObjType type) {
	Obj* object = (Obj*)reallocate(vm, NULL, 0, size);
	object->type = type;
	object->isMarked = false;

	object->next = vm->objects;
	vm->objects = object;

#ifdef DEBUG_LOG_GC
	printf("%p allocate %zu for %d\n", (void*)object, size, type);
#endif

	return object;
}

ObjClass* newClass(VM* vm, ObjString* name) {
	ObjClass* _class = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
	_class->name = name;
	return _class;
}

ObjClosure* newClosure(VM* vm, ObjFunction* function) {
	ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);
	for (int i = 0; i < function->upvalueCount; i++) {
		upvalues[i] = NULL;
	}

	ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = function->upvalueCount;
	return closure;
}

ObjFunction* newFunction(VM* vm) {
	ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
	function->arity = 0;
	function->upvalueCount = 0;
	function->name = NULL;
	initChunk(&function->chunk);
	return function;
}

ObjInstance* newInstance(VM* vm, ObjClass* _class) {
	ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
	instance->_class = _class;
	initTable(&instance->fields);
	return instance;
}

ObjNative* newNative(VM* vm, NativeFn function) {
	ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
	native->function = function;
	return native;
}

static ObjString* allocateString(VM* vm, char* chars, int length, uint32_t hash) {
	ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
	string->length = length;
	string->chars = chars;
	string->hash = hash;
	push(vm, OBJ_VAL(string));
	tableSet(vm, &vm->strings, string, NIL_VAL);
	pop(vm);
	return string;
}

static uint32_t hashString(const char* key, int length) {
	uint32_t hash = 2166136261u;
	for (int i = 0; i < length; i++) {
		hash ^= (uint8_t)key[i];
		hash *= 16777619;
	}
	return hash;
}

ObjString* takeString(VM* vm, char* chars, int length) {
	uint32_t hash = hashString(chars, length);
	ObjString* interned = tableFindString(&vm->strings, chars, length, hash);
	if (interned != NULL) {
		FREE_ARRAY(char, chars, length + 1);
		return interned;
	}

	return allocateString(vm, chars, length, hash);
}

ObjString* copyString(VM* vm, const char* chars, int length) {
	uint32_t hash = hashString(chars, length);
	ObjString* interned = tableFindString(&vm->strings, chars, length, hash);
	if (interned != NULL) return interned;

	char* heapChars = ALLOCATE(char, length + 1);
	memcpy(heapChars, chars, length);
	heapChars[length] = '\0';
	return allocateString(vm, heapChars, length, hash);
}

ObjString* copyEscapeString(VM* vm, const char* chars, int length) {
	// count actual characters
	int escapedLength = 0;
	for (int i = 0; i < length; i++) {
		if (chars[i] == '\\') {
			i++;
		}
		escapedLength++;
	}

	// use the base copy string if no escaped characters
	if (escapedLength == length) {
		return copyString(vm, chars, length);
	}

	// allocate the actual size
	char* heapChars = ALLOCATE(char, escapedLength + 1);
	int index = 0;
	for (int i = 0; i < length; i++) {
		if (chars[i] == '\\') {
			i++;
			switch (chars[i]) {
			case 'n': heapChars[index++] = '\n'; break;
			case 'r': heapChars[index++] = '\r'; break;
			case 't': heapChars[index++] = '\t'; break;
			default:  heapChars[index++] = chars[i]; break;
			}
		}
		else {
			heapChars[index++] = chars[i];
		}
	}
	heapChars[escapedLength] = '\0';
	return takeString(vm, heapChars, escapedLength);
}

ObjUpvalue* newUpvalue(VM* vm, Value* slot) {
	ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
	upvalue->location = slot;
	upvalue->closed = NIL_VAL;
	upvalue->next = NULL;
	return upvalue;
}

static void printFunction(ObjFunction* function) {
	if (function->name != NULL) {
		printf("<fn %s>", function->name->chars);
	}
	else {
		printf("<script>");
	}
}

void printObject(Value value) {
	switch (OBJ_TYPE(value)) {
	case OBJ_CLASS: {
		ObjClass* _class = AS_CLASS(value);
		if (_class->name != NULL) {
			printf("class %s", _class->name->chars);
		}
		else {
			printf("anonymous class");
		}
		break;
	}
	case OBJ_CLOSURE:	printFunction(AS_CLOSURE(value)->function); break;
	case OBJ_FUNCTION:	printFunction(AS_FUNCTION(value)); break;
	case OBJ_INSTANCE: {
		ObjInstance* instance = AS_INSTANCE(value);
		if (instance->_class->name != NULL) {
			printf("%s instance", AS_INSTANCE(value)->_class->name->chars);
		}
		else {
			printf("anonymous class instance");
		}
		break;
	}
	case OBJ_NATIVE:	printf("<native fn>"); break;
	case OBJ_STRING:	printf("%s", AS_CSTRING(value)); break;
	case OBJ_UPVALUE:	printf("upvalue"); break;
	}
}
