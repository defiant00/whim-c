#ifndef whimsy_vm_h
#define whimsy_vm_h

#include "compiler.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
	ObjClosure* closure;
	uint8_t* ip;
	Value* slots;
} CallFrame;

struct VM {
	CallFrame frames[FRAMES_MAX];
	int frameCount;
	Value stack[STACK_MAX];
	Value* stackTop;
	Table globals;
	Table strings;
	ObjUpvalue* openUpvalues;
	size_t bytesAllocated;
	size_t nextGC;
	Obj* objects;

	Compiler* compiler;
	Parser parser;
	Scanner scanner;

	int grayCount;
	int grayCapacity;
	Obj** grayStack;
};

typedef enum {
	INTERPRET_OK,
	INTERPRET_COMPILE_ERROR,
	INTERPRET_RUNTIME_ERROR,
} InterpretResult;

void initVM(VM* vm);
void freeVM(VM* vm);
InterpretResult interpret(VM* vm, const char* source);
void push(VM* vm, Value value);
Value pop(VM* vm);

#endif
