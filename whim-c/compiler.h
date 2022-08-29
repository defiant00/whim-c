#ifndef whimsy_compiler_h
#define whimsy_compiler_h

#include "object.h"
#include "scanner.h"
#include "vm.h"

typedef struct {
	Scanner scanner;
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
} Local;

typedef struct {
	int start;
	int exit;
	int depth;
} Loop;

typedef enum {
	TYPE_FUNCTION,
	TYPE_SCRIPT,
} FunctionType;

typedef struct {
	VM* vm;
	Parser parser;
	ObjFunction* function;
	FunctionType type;
	Local locals[UINT8_COUNT];
	int localCount;
	Loop loops[MAX_LOOP];
	int loopCount;
	int scopeDepth;
} Compiler;

typedef void(*ParseFn)(Compiler*);

typedef struct {
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

ObjFunction* compile(VM* vm, const char* source);

#endif
