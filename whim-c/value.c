#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"

void initValueArray(ValueArray* array) {
	array->values = NULL;
	array->capacity = 0;
	array->count = 0;
}

void writeValueArray(VM* vm, ValueArray* array, Value value) {
	if (array->capacity < array->count + 1) {
		int oldCapacity = array->capacity;
		array->capacity = GROW_CAPACITY(oldCapacity);
		array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
	}

	array->values[array->count] = value;
	array->count++;
}

void freeValueArray(VM* vm, ValueArray* array) {
	FREE_ARRAY(Value, array->values, array->capacity);
	initValueArray(array);
}

void printValue(Value value) {
	switch (VAL_TYPE(value)) {
	case VAL_BOOL: printf(AS_BOOL(value) ? "true" : "false"); break;
	case VAL_NIL: printf("nil"); break;
	case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;
	case VAL_OBJ: printObject(value); break;
	}
}

bool valuesEqual(Value a, Value b) {
	if (VAL_TYPE(a) != VAL_TYPE(b)) return false;
	switch (VAL_TYPE(a)) {
	case VAL_BOOL:		return AS_BOOL(a) == AS_BOOL(b);
	case VAL_NIL:		return true;
	case VAL_NUMBER:	return AS_NUMBER(a) == AS_NUMBER(b);
	case VAL_OBJ:		return AS_OBJ(a) == AS_OBJ(b);
	default:			return false; // unreachable
	}
}
