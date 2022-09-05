#ifndef whimsy_compiler_h
#define whimsy_compiler_h

#include "object.h"
#include "scanner.h"

typedef struct VM VM;

typedef struct {
	Token current;
	Token previous;
	bool hadError;
	bool panicMode;
} Parser;

typedef enum {
	PREC_NONE,
	PREC_OR,			// or
	PREC_AND,			// and
	PREC_EQUALITY,		// == !=
	PREC_COMPARISON,	// < > <= >=
	PREC_TERM,			// + -
	PREC_FACTOR,		// * / %
	PREC_UNARY,			// ! -
	PREC_CALL,			// . () []
	PREC_PRIMARY,
} Precedence;

typedef struct {
	Token name;
	bool constant;
	int depth;
	bool isCaptured;
} Local;

typedef struct {
	uint8_t index;
	bool isLocal;
} Upvalue;

typedef struct {
	int start;
	int exit;
	int depth;
} Loop;

typedef enum {
	TYPE_FUNCTION,
	TYPE_SCRIPT,
} FunctionType;

typedef struct Compiler {
	struct Compiler* enclosing;
	ObjFunction* function;
	FunctionType type;
	bool isNamedDeclaration;
	const char* nameStart;
	int nameLength;
	Local locals[UINT8_COUNT];
	int localCount;
	Upvalue upvalues[UINT8_COUNT];
	Loop loops[MAX_LOOP];
	int loopCount;
	int scopeDepth;
} Compiler;

typedef void(*ParseFn)(VM*);

typedef struct {
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

ObjFunction* compile(VM* vm, const char* source);
void markCompilerRoots(VM* vm);

#endif
