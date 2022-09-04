#ifndef whimsy_value_h
#define whimsy_value_h

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;
typedef struct VM VM;

typedef enum {
	VAL_BOOL,
	VAL_NIL,
	VAL_NUMBER,
	VAL_OBJ,
	VAL_TYPE_MASK = 7,	// must be one less than a power of 2
	VAL_CONSTANT,
} ValueType;

typedef struct {
	ValueType type;
	union {
		bool boolean;
		double number;
		Obj* obj;
	} as;
} Value;

#define VAL_TYPE(value)		((value).type & VAL_TYPE_MASK)
#define IS_CONST(value)		((value).type & VAL_CONSTANT)
#define AS_CONST(value)		asConst(value)
#define AS_VAR(value)		asVar(value)

#define IS_BOOL(value)		(VAL_TYPE(value) == VAL_BOOL)
#define IS_NIL(value)		(VAL_TYPE(value) == VAL_NIL)
#define IS_NUMBER(value)	(VAL_TYPE(value) == VAL_NUMBER)
#define IS_OBJ(value)		(VAL_TYPE(value) == VAL_OBJ)

#define AS_OBJ(value)		((value).as.obj)
#define AS_BOOL(value)		((value).as.boolean)
#define AS_NUMBER(value)	((value).as.number)

#define BOOL_VAL(value)		((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL				((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value)	((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)		((Value){VAL_OBJ, {.obj = (Obj*)object}})

static inline Value asConst(Value value) {
	value.type |= VAL_CONSTANT;
	return value;
}

static inline Value asVar(Value value) {
	value.type &= VAL_TYPE_MASK;
	return value;
}

typedef struct {
	int capacity;
	int count;
	Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(VM* vm, ValueArray* array, Value value);
void freeValueArray(VM* vm, ValueArray* array);

void printValue(Value value);

#endif
