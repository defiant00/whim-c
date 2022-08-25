#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
	Scanner scanner;
	Chunk* compilingChunk;
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
	int depth;
} Local;

typedef struct {
	VM* vm;
	Parser parser;
	Local locals[UINT8_COUNT];
	int localCount;
	int scopeDepth;
} Compiler;

typedef void(*ParseFn)(Compiler*);

typedef struct {
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

static Chunk* currentChunk(Compiler* compiler) {
	return compiler->parser.compilingChunk;
}

static void initCompiler(Compiler* compiler, VM* vm, const char* source, Chunk* chunk) {
	compiler->vm = vm;

	compiler->localCount = 0;
	compiler->scopeDepth = 0;

	initScanner(&compiler->parser.scanner, source);

	compiler->parser.compilingChunk = chunk;
	compiler->parser.hadError = false;
	compiler->parser.panicMode = false;
}

static void errorAt(Compiler* compiler, Token* token, const char* message) {
	if (compiler->parser.panicMode) return;
	compiler->parser.panicMode = true;

	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF) {
		fprintf(stderr, " at end");
	}
	else if (token->type == TOKEN_ERROR) {
		// nothing
	}
	else {
		fprintf(stderr, " at '%.*s'", token->length, token->start);
	}

	fprintf(stderr, ": %s\n", message);
	compiler->parser.hadError = true;
}

static void error(Compiler* compiler, const char* message) {
	errorAt(compiler, &compiler->parser.previous, message);
}

static void errorAtCurrent(Compiler* compiler, const char* message) {
	errorAt(compiler, &compiler->parser.current, message);
}

static void advance(Compiler* compiler) {
	compiler->parser.previous = compiler->parser.current;

	for (;;) {
		compiler->parser.current = scanToken(&compiler->parser.scanner);
		if (compiler->parser.current.type != TOKEN_ERROR) break;

		errorAtCurrent(compiler, compiler->parser.current.start);
	}
}

static void consume(Compiler* compiler, TokenType type, const char* message) {
	if (compiler->parser.current.type == type) {
		advance(compiler);
		return;
	}

	errorAtCurrent(compiler, message);
}

static bool check(Compiler* compiler, TokenType type) {
	return compiler->parser.current.type == type;
}

static bool match(Compiler* compiler, TokenType type) {
	if (!check(compiler, type)) return false;
	advance(compiler);
	return true;
}

static void emitByte(Compiler* compiler, uint8_t byte) {
	writeChunk(currentChunk(compiler), byte, compiler->parser.previous.line);
}

static void emitBytes(Compiler* compiler, uint8_t byte1, uint8_t byte2) {
	emitByte(compiler, byte1);
	emitByte(compiler, byte2);
}

static void emitReturn(Compiler* compiler) {
	emitByte(compiler, OP_RETURN);
}

static uint8_t makeConstant(Compiler* compiler, Value value) {
	int constant = addConstant(currentChunk(compiler), value);
	if (constant > UINT8_MAX) {
		error(compiler, "Too many constants in one chunk.");
		return 0;
	}

	return (uint8_t)constant;
}

static void emitConstant(Compiler* compiler, Value value) {
	emitBytes(compiler, OP_CONSTANT, makeConstant(compiler, value));
}

static void endCompiler(Compiler* compiler) {
#ifdef DEBUG_PRINT_CODE
	if (!compiler->parser.hadError) {
		disassembleChunk(currentChunk(compiler), "code");
	}
#endif
	emitReturn(compiler);
}

static void expression(Compiler* compiler);
static void statement(Compiler* compiler);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Compiler* compiler, Precedence precedence);

static uint8_t identifierConstant(Compiler* compiler, Token* name) {
	return makeConstant(compiler, OBJ_VAL(copyString(compiler->vm, name->start, name->length)));
}

static void defineVariable(Compiler* compiler, uint8_t global, bool constant) {
	emitBytes(compiler, constant ? OP_DEFINE_GLOBAL_CONST : OP_DEFINE_GLOBAL_VAR, global);
}

static void binary(Compiler* compiler) {
	TokenType operatorType = compiler->parser.previous.type;
	ParseRule* rule = getRule(operatorType);
	parsePrecedence(compiler, (Precedence)(rule->precedence + 1));

	switch (operatorType) {
	case TOKEN_BANG_EQUAL:		emitByte(compiler, OP_NOT_EQUAL); break;
	case TOKEN_EQUAL_EQUAL:		emitByte(compiler, OP_EQUAL); break;
	case TOKEN_GREATER:			emitByte(compiler, OP_GREATER); break;
	case TOKEN_GREATER_EQUAL:	emitByte(compiler, OP_GREATER_EQUAL); break;
	case TOKEN_LESS:			emitByte(compiler, OP_LESS); break;
	case TOKEN_LESS_EQUAL:		emitByte(compiler, OP_LESS_EQUAL); break;
	case TOKEN_PLUS:			emitByte(compiler, OP_ADD); break;
	case TOKEN_MINUS:			emitByte(compiler, OP_SUBTRACT); break;
	case TOKEN_STAR:			emitByte(compiler, OP_MULTIPLY); break;
	case TOKEN_SLASH:			emitByte(compiler, OP_DIVIDE); break;
	case TOKEN_PERCENT:			emitByte(compiler, OP_MODULUS); break;
	default: return; // unreachable
	}
}

static void literal(Compiler* compiler) {
	switch (compiler->parser.previous.type) {
	case TOKEN_FALSE: emitByte(compiler, OP_FALSE); break;
	case TOKEN_NIL: emitByte(compiler, OP_NIL); break;
	case TOKEN_TRUE: emitByte(compiler, OP_TRUE); break;
	default: return; // unreachable
	}
}

static void grouping(Compiler* compiler) {
	expression(compiler);
	consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(Compiler* compiler) {
	double value = strtod(compiler->parser.previous.start, NULL);
	emitConstant(compiler, NUMBER_VAL(value));
}

static void string(Compiler* compiler) {
	// TODO - deal with escaped characters
	emitConstant(compiler, OBJ_VAL(copyString(compiler->vm,
		compiler->parser.previous.start + 1,
		compiler->parser.previous.length - 2)));
}

static void namedVariable(Compiler* compiler, Token name) {
	uint8_t arg = identifierConstant(compiler, &name);
	emitBytes(compiler, OP_GET_GLOBAL, arg);
}

static void variable(Compiler* compiler) {
	namedVariable(compiler, compiler->parser.previous);
}

static void unary(Compiler* compiler) {
	TokenType operatorType = compiler->parser.previous.type;

	// compile the operand
	parsePrecedence(compiler, PREC_UNARY);

	// emit the operator instruction
	switch (operatorType)
	{
	case TOKEN_BANG: emitByte(compiler, OP_NOT); break;
	case TOKEN_MINUS: emitByte(compiler, OP_NEGATE); break;
	default: return; // unreachable
	}
}

ParseRule rules[] = {
	[TOKEN_LEFT_PAREN] = {		grouping,	NULL,		PREC_NONE},
	[TOKEN_RIGHT_PAREN] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_LEFT_BRACKET] = {	NULL,		NULL,		PREC_NONE},
	[TOKEN_RIGHT_BRACKET] = {	NULL,		NULL,		PREC_NONE},
	[TOKEN_COMMA] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_DOT] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_SEMICOLON] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_UNDERSCORE] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_COLON_COLON] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_COLON_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_BANG] = {			unary,		NULL,		PREC_NONE},
	[TOKEN_BANG_EQUAL] = {		NULL,		binary,		PREC_EQUALITY},
	[TOKEN_EQUAL] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_EQUAL_EQUAL] = {		NULL,		binary,		PREC_EQUALITY},
	[TOKEN_LESS] = {			NULL,		binary,		PREC_COMPARISON},
	[TOKEN_LESS_EQUAL] = {		NULL,		binary,		PREC_COMPARISON},
	[TOKEN_GREATER] = {			NULL,		binary,		PREC_COMPARISON},
	[TOKEN_GREATER_EQUAL] = {	NULL,		binary,		PREC_COMPARISON},
	[TOKEN_PLUS] = {			NULL,		binary,		PREC_TERM},
	[TOKEN_PLUS_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_MINUS] = {			unary,		binary,		PREC_TERM},
	[TOKEN_MINUS_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_STAR] = {			NULL,		binary,		PREC_FACTOR},
	[TOKEN_STAR_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_SLASH] = {			NULL,		binary,		PREC_FACTOR},
	[TOKEN_SLASH_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_PERCENT] = {			NULL,		binary,		PREC_FACTOR},
	[TOKEN_PERCENT_EQUAL] = {	NULL,		NULL,		PREC_NONE},
	[TOKEN_IDENTIFIER] = {		variable,	NULL,		PREC_NONE},
	[TOKEN_STRING] = {			string,		NULL,		PREC_NONE},
	[TOKEN_NUMBER] = {			number,		NULL,		PREC_NONE},
	[TOKEN_AND] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_BREAK] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_CATCH] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_CLASS] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_CONTINUE] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_DO] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_ELSE] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FALSE] = {			literal,	NULL,		PREC_NONE},
	[TOKEN_FINALLY] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FN] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_FOR] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_FROM] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_IF] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_IN] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_IS] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_NIL] = {				literal,	NULL,		PREC_NONE},
	[TOKEN_OR] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_RETURN] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_THROW] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_TRUE] = {			literal,	NULL,		PREC_NONE},
	[TOKEN_TRY] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_CLASS_END] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_DO_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FN_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FOR_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_IF_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_TRY_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_ERROR] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_EOF] = {				NULL,		NULL,		PREC_NONE},
};

static void parsePrecedenceFromPrevious(Compiler* compiler, Precedence precedence) {
	ParseFn prefixRule = getRule(compiler->parser.previous.type)->prefix;
	if (prefixRule == NULL) {
		error(compiler, "Expect expression.");
		return;
	}

	prefixRule(compiler);

	while (precedence <= getRule(compiler->parser.current.type)->precedence) {
		advance(compiler);
		ParseFn infixRule = getRule(compiler->parser.previous.type)->infix;
		infixRule(compiler);
	}
}

static void parsePrecedence(Compiler* compiler, Precedence precedence) {
	advance(compiler);
	parsePrecedenceFromPrevious(compiler, precedence);
}

static ParseRule* getRule(TokenType type) {
	return &rules[type];
}

static void expression(Compiler* compiler) {
	parsePrecedence(compiler, PREC_OR);
}

static void expressionFromPrevious(Compiler* compiler) {
	parsePrecedenceFromPrevious(compiler, PREC_OR);
}

static void expressionStatement(Compiler* compiler) {
	// parse one primary expression and check for declaration or assignment
	// if not, then parse as a normal expression statement
	if (match(compiler, TOKEN_IDENTIFIER)) {
		switch (compiler->parser.current.type) {
		case TOKEN_COLON_COLON:
		{
			uint8_t arg = identifierConstant(compiler, &compiler->parser.previous);
			advance(compiler);	// accept ::
			expression(compiler);
			defineVariable(compiler, arg, true);
			break;
		}
		case TOKEN_COLON_EQUAL:
		{
			uint8_t arg = identifierConstant(compiler, &compiler->parser.previous);
			advance(compiler);	// accept :=
			expression(compiler);
			defineVariable(compiler, arg, false);
			break;
		}
		case TOKEN_EQUAL:
		case TOKEN_PLUS_EQUAL:
		case TOKEN_MINUS_EQUAL:
		case TOKEN_STAR_EQUAL:
		case TOKEN_SLASH_EQUAL:
		case TOKEN_PERCENT_EQUAL: {
			// assignment
			uint8_t arg = identifierConstant(compiler, &compiler->parser.previous);

			uint8_t op = OP_SET_GLOBAL;
			switch (compiler->parser.current.type) {
			case TOKEN_PLUS_EQUAL: op = OP_ADD_SET_GLOBAL; break;
			case TOKEN_MINUS_EQUAL: op = OP_SUBTRACT_SET_GLOBAL; break;
			case TOKEN_STAR_EQUAL: op = OP_MULTIPLY_SET_GLOBAL; break;
			case TOKEN_SLASH_EQUAL: op = OP_DIVIDE_SET_GLOBAL; break;
			case TOKEN_PERCENT_EQUAL: op = OP_MODULUS_SET_GLOBAL; break;
			}

			advance(compiler);	// accept = += -= *= /= %=
			expression(compiler);
			emitBytes(compiler, op, arg);
			break;
		}
		default:
			// not an assignment, continue parsing the expression
			expressionFromPrevious(compiler);
			emitByte(compiler, OP_POP);
			break;
		}
	}
	else {
		expression(compiler);
		emitByte(compiler, OP_POP);
	}
}

static void synchronize(Compiler* compiler) {
	compiler->parser.panicMode = false;

	while (compiler->parser.current.type != TOKEN_EOF) {
		switch (compiler->parser.previous.type) {
		case TOKEN_CLASS_END:
		case TOKEN_DO_END:
		case TOKEN_FN_END:
		case TOKEN_FOR_END:
		case TOKEN_IF_END:
		case TOKEN_SEMICOLON:
		case TOKEN_TRY_END:
			return;
		}

		switch (compiler->parser.current.type) {
		case TOKEN_BREAK:
		case TOKEN_CATCH:
		case TOKEN_CLASS:
		case TOKEN_CONTINUE:
		case TOKEN_DO:
		case TOKEN_FINALLY:
		case TOKEN_FN:
		case TOKEN_FOR:
		case TOKEN_IF:
		case TOKEN_RETURN:
		case TOKEN_THROW:
		case TOKEN_TRY:
			return;
		}

		advance(compiler);
	}
}

static void statement(Compiler* compiler) {
	if (match(compiler, TOKEN_SEMICOLON)) {
		// empty statement
	}
	else {
		expressionStatement(compiler);
	}

	if (compiler->parser.panicMode) synchronize(compiler);
}

bool compile(VM* vm, const char* source, Chunk* chunk) {
	Compiler compiler;

	initCompiler(&compiler, vm, source, chunk);

	advance(&compiler);

	while (!match(&compiler, TOKEN_EOF)) {
		statement(&compiler);
	}

	endCompiler(&compiler);
	return !compiler.parser.hadError;
}
