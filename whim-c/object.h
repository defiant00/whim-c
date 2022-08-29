#ifndef whimsy_object_h
#define whimsy_object_h

#include "chunk.h"
#include "common.h"
#include "value.h"
#include "vm.h"

#define OBJ_TYPE(value)		(AS_OBJ(value)->type)

#define IS_FUNCTION(value)	isObjType(value, OBJ_FUNCTION)
#define IS_STRING(value)	isObjType(value, OBJ_STRING)

#define AS_FUNCTION(value)	((ObjFunction*)AS_OBJ(value))
#define AS_STRING(value)	((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)	(((ObjString*)AS_OBJ(value))->chars)

typedef enum {
	OBJ_FUNCTION,
	OBJ_STRING,
} ObjType;

struct Obj {
	ObjType type;
	struct Obj* next;
};

typedef struct {
	Obj obj;
	int arity;
	Chunk chunk;
	ObjString* name;
} ObjFunction;

struct ObjString {
	Obj obj;
	int length;
	char* chars;
	uint32_t hash;
};

ObjFunction* newFunction(VM* vm);
ObjString* takeString(VM* vm, char* chars, int length);
ObjString* copyString(VM* vm, const char* chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
	return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
