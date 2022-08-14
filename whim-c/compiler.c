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

static Chunk* currentChunk(Parser* parser) {
	return parser->compilingChunk;
}

typedef enum {
	PREC_NONE,
	PREC_ASSIGN,		// = += -= *= /=
	PREC_OR,			// or
	PREC_AND,			// and
	PREC_EQUALITY,		// == !=
	PREC_COMPARISON,	// < > <= >=
	PREC_TERM,			// + -
	PREC_FACTOR,		// * /
	PREC_UNARY,			// ! -
	PREC_CALL,			// . () []
	PREC_PRIMARY,
} Precedence;

typedef void(*ParseFn)(Parser*);

typedef struct {
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

static void errorAt(Parser* parser, Token* token, const char* message) {
	if (parser->panicMode) return;
	parser->panicMode = true;

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
	parser->hadError = true;
}

static void error(Parser* parser, const char* message) {
	errorAt(parser, &parser->previous, message);
}

static void errorAtCurrent(Parser* parser, const char* message) {
	errorAt(parser, &parser->current, message);
}

static void advance(Parser* parser) {
	parser->previous = parser->current;

	for (;;) {
		parser->current = scanToken(&parser->scanner);
		if (parser->current.type != TOKEN_ERROR) break;

		errorAtCurrent(parser, parser->current.start);
	}
}

static void consume(Parser* parser, TokenType type, const char* message) {
	if (parser->current.type == type) {
		advance(parser);
		return;
	}

	errorAtCurrent(parser, message);
}

static void emitByte(Parser* parser, uint8_t byte) {
	writeChunk(currentChunk(parser), byte, parser->previous.line);
}

static void emitBytes(Parser* parser, uint8_t byte1, uint8_t byte2) {
	emitByte(parser, byte1);
	emitByte(parser, byte2);
}

static void emitReturn(Parser* parser) {
	emitByte(parser, OP_RETURN);
}

static uint8_t makeConstant(Parser* parser, Value value) {
	int constant = addConstant(currentChunk(parser), value);
	if (constant > UINT8_MAX) {
		error(parser, "Too many constants in one chunk.");
		return 0;
	}

	return (uint8_t)constant;
}

static void emitConstant(Parser* parser, Value value) {
	emitBytes(parser, OP_CONSTANT, makeConstant(parser, value));
}

static void endCompiler(Parser* parser) {
#ifdef DEBUG_PRINT_CODE
	if (!parser->hadError) {
		disassembleChunk(currentChunk(parser), "code");
	}
#endif
	emitReturn(parser);
}

static void expression(Parser* parser);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Parser* parser, Precedence precedence);

static void binary(Parser* parser) {
	TokenType operatorType = parser->previous.type;
	ParseRule* rule = getRule(operatorType);
	parsePrecedence(parser, (Precedence)(rule->precedence + 1));

	switch (operatorType) {
	case TOKEN_PLUS:	emitByte(parser, OP_ADD); break;
	case TOKEN_MINUS:	emitByte(parser, OP_SUBTRACT); break;
	case TOKEN_STAR:	emitByte(parser, OP_MULTIPLY); break;
	case TOKEN_SLASH:	emitByte(parser, OP_DIVIDE); break;
	default: return; // unreachable
	}
}

static void grouping(Parser* parser) {
	expression(parser);
	consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(Parser* parser) {
	double value = strtod(parser->previous.start, NULL);
	emitConstant(parser, value);
}

static void unary(Parser* parser) {
	TokenType operatorType = parser->previous.type;

	// compile the operand
	parsePrecedence(parser, PREC_UNARY);

	// emit the operator instruction
	switch (operatorType)
	{
	case TOKEN_MINUS:
		emitByte(parser, OP_NEGATE);
		break;
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
	[TOKEN_BANG] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_BANG_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_EQUAL] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_EQUAL_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_LESS] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_LESS_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_GREATER] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_GREATER_EQUAL] = {	NULL,		NULL,		PREC_NONE},
	[TOKEN_MINUS] = {			unary,		binary,		PREC_TERM},
	[TOKEN_MINUS_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_PLUS] = {			NULL,		binary,		PREC_TERM},
	[TOKEN_PLUS_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_SLASH] = {			NULL,		binary,		PREC_FACTOR},
	[TOKEN_SLASH_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_STAR] = {			NULL,		binary,		PREC_FACTOR},
	[TOKEN_STAR_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_COLON_COLON] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_COLON_EQUAL] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_IDENTIFIER] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_STRING] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_NUMBER] = {			number,		NULL,		PREC_NONE},
	[TOKEN_AND] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_BASE] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_CLASS] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_ELSE] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FALSE] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FN] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_FOR] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_FROM] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_IF] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_IN] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_IS] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_NIL] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_OR] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_RETURN] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_TRUE] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_CLASS_END] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_FN_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FOR_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_IF_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_ERROR] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_EOF] = {				NULL,		NULL,		PREC_NONE},
};

static void parsePrecedence(Parser* parser, Precedence precedence) {
	advance(parser);
	ParseFn prefixRule = getRule(parser->previous.type)->prefix;
	if (prefixRule == NULL) {
		error(parser, "Expect expression.");
		return;
	}

	prefixRule(parser);

	while (precedence <= getRule(parser->current.type)->precedence) {
		advance(parser);
		ParseFn infixRule = getRule(parser->previous.type)->infix;
		infixRule(parser);
	}
}

static ParseRule* getRule(TokenType type) {
	return &rules[type];
}

static void expression(Parser* parser) {
	parsePrecedence(parser, PREC_ASSIGN);
}

bool compile(const char* source, Chunk* chunk) {
	Parser parser;
	initScanner(&parser.scanner, source);

	parser.compilingChunk = chunk;
	parser.hadError = false;
	parser.panicMode = false;

	advance(&parser);
	expression(&parser);
	consume(&parser, TOKEN_EOF, "Expect end of expression.");

	endCompiler(&parser);
	return !parser.hadError;
}
