#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
	VM* vm;
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

static bool check(Parser* parser, TokenType type) {
	return parser->current.type == type;
}

static bool match(Parser* parser, TokenType type) {
	if (!check(parser, type)) return false;
	advance(parser);
	return true;
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
static void statement(Parser* parser);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Parser* parser, Precedence precedence);

static uint8_t identifierConstant(Parser* parser, Token* name) {
	return makeConstant(parser, OBJ_VAL(copyString(parser->vm, name->start, name->length)));
}

static void defineVariable(Parser* parser, uint8_t global, bool constant) {
	emitBytes(parser, constant ? OP_DEFINE_GLOBAL_CONST : OP_DEFINE_GLOBAL_VAR, global);
}

static void binary(Parser* parser) {
	TokenType operatorType = parser->previous.type;
	ParseRule* rule = getRule(operatorType);
	parsePrecedence(parser, (Precedence)(rule->precedence + 1));

	switch (operatorType) {
	case TOKEN_BANG_EQUAL:		emitByte(parser, OP_NOT_EQUAL); break;
	case TOKEN_EQUAL_EQUAL:		emitByte(parser, OP_EQUAL); break;
	case TOKEN_GREATER:			emitByte(parser, OP_GREATER); break;
	case TOKEN_GREATER_EQUAL:	emitByte(parser, OP_GREATER_EQUAL); break;
	case TOKEN_LESS:			emitByte(parser, OP_LESS); break;
	case TOKEN_LESS_EQUAL:		emitByte(parser, OP_LESS_EQUAL); break;
	case TOKEN_PLUS:			emitByte(parser, OP_ADD); break;
	case TOKEN_MINUS:			emitByte(parser, OP_SUBTRACT); break;
	case TOKEN_STAR:			emitByte(parser, OP_MULTIPLY); break;
	case TOKEN_SLASH:			emitByte(parser, OP_DIVIDE); break;
	case TOKEN_PERCENT:			emitByte(parser, OP_MODULUS); break;
	default: return; // unreachable
	}
}

static void literal(Parser* parser) {
	switch (parser->previous.type) {
	case TOKEN_FALSE: emitByte(parser, OP_FALSE); break;
	case TOKEN_NIL: emitByte(parser, OP_NIL); break;
	case TOKEN_TRUE: emitByte(parser, OP_TRUE); break;
	default: return; // unreachable
	}
}

static void grouping(Parser* parser) {
	expression(parser);
	consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(Parser* parser) {
	double value = strtod(parser->previous.start, NULL);
	emitConstant(parser, NUMBER_VAL(value));
}

static void string(Parser* parser) {
	// TODO - deal with escaped characters
	emitConstant(parser, OBJ_VAL(copyString(parser->vm,
		parser->previous.start + 1,
		parser->previous.length - 2)));
}

static void namedVariable(Parser* parser, Token name) {
	uint8_t arg = identifierConstant(parser, &name);
	emitBytes(parser, OP_GET_GLOBAL, arg);
}

static void variable(Parser* parser) {
	namedVariable(parser, parser->previous);
}

static void unary(Parser* parser) {
	TokenType operatorType = parser->previous.type;

	// compile the operand
	parsePrecedence(parser, PREC_UNARY);

	// emit the operator instruction
	switch (operatorType)
	{
	case TOKEN_BANG: emitByte(parser, OP_NOT); break;
	case TOKEN_MINUS: emitByte(parser, OP_NEGATE); break;
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
	[TOKEN_CLASS] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_CONTINUE] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_DO] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_ELSE] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FALSE] = {			literal,	NULL,		PREC_NONE},
	[TOKEN_FN] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_FOR] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_FROM] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_IF] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_IN] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_IS] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_NIL] = {				literal,	NULL,		PREC_NONE},
	[TOKEN_OR] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_RETURN] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_TRUE] = {			literal,	NULL,		PREC_NONE},
	[TOKEN_CLASS_END] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_DO_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FN_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FOR_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_IF_END] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_ERROR] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_EOF] = {				NULL,		NULL,		PREC_NONE},
};

static void parsePrecedenceFromPrevious(Parser* parser, Precedence precedence) {
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

static void parsePrecedence(Parser* parser, Precedence precedence) {
	advance(parser);
	parsePrecedenceFromPrevious(parser, precedence);
}

static ParseRule* getRule(TokenType type) {
	return &rules[type];
}

static void expression(Parser* parser) {
	parsePrecedence(parser, PREC_OR);
}

static void expressionFromPrevious(Parser* parser) {
	parsePrecedenceFromPrevious(parser, PREC_OR);
}

static void expressionStatement(Parser* parser) {
	// parse one primary expression and check for declaration or assignment
	// if not, then parse as a normal expression statement
	if (match(parser, TOKEN_IDENTIFIER)) {
		switch (parser->current.type) {
		case TOKEN_COLON_COLON:
		{
			uint8_t arg = identifierConstant(parser, &parser->previous);
			advance(parser);	// accept ::
			expression(parser);
			defineVariable(parser, arg, true);
			break;
		}
		case TOKEN_COLON_EQUAL:
		{
			uint8_t arg = identifierConstant(parser, &parser->previous);
			advance(parser);	// accept :=
			expression(parser);
			defineVariable(parser, arg, false);
			break;
		}
		case TOKEN_EQUAL:
		case TOKEN_PLUS_EQUAL:
		case TOKEN_MINUS_EQUAL:
		case TOKEN_STAR_EQUAL:
		case TOKEN_SLASH_EQUAL:
		case TOKEN_PERCENT_EQUAL: {
			// assignment
			uint8_t arg = identifierConstant(parser, &parser->previous);

			uint8_t op = OP_SET_GLOBAL;
			switch (parser->current.type) {
			case TOKEN_PLUS_EQUAL: op = OP_ADD_SET_GLOBAL; break;
			case TOKEN_MINUS_EQUAL: op = OP_SUBTRACT_SET_GLOBAL; break;
			case TOKEN_STAR_EQUAL: op = OP_MULTIPLY_SET_GLOBAL; break;
			case TOKEN_SLASH_EQUAL: op = OP_DIVIDE_SET_GLOBAL; break;
			case TOKEN_PERCENT_EQUAL: op = OP_MODULUS_SET_GLOBAL; break;
			}

			advance(parser);	// accept = += -= *= /= %=
			expression(parser);
			emitBytes(parser, op, arg);
			break;
		}
		default:
			// not an assignment, continue parsing the expression
			expressionFromPrevious(parser);
			emitByte(parser, OP_POP);
			break;
		}
	}
	else {
		expression(parser);
		emitByte(parser, OP_POP);
	}
}

static void synchronize(Parser* parser) {
	parser->panicMode = false;

	while (parser->current.type != TOKEN_EOF) {
		switch (parser->previous.type) {
		case TOKEN_CLASS_END:
		case TOKEN_DO_END:
		case TOKEN_FN_END:
		case TOKEN_FOR_END:
		case TOKEN_IF_END:
		case TOKEN_SEMICOLON:
			return;
		}

		switch (parser->current.type) {
		case TOKEN_BREAK:
		case TOKEN_CLASS:
		case TOKEN_CONTINUE:
		case TOKEN_DO:
		case TOKEN_FN:
		case TOKEN_FOR:
		case TOKEN_IF:
		case TOKEN_RETURN:
			return;
		}

		advance(parser);
	}
}

static void statement(Parser* parser) {
	if (match(parser, TOKEN_SEMICOLON)) {
		// empty statement
	}
	else {
		expressionStatement(parser);
	}

	if (parser->panicMode) synchronize(parser);
}

bool compile(VM* vm, const char* source, Chunk* chunk) {
	Parser parser;
	initScanner(&parser.scanner, source);

	parser.vm = vm;
	parser.compilingChunk = chunk;
	parser.hadError = false;
	parser.panicMode = false;

	advance(&parser);

	while (!match(&parser, TOKEN_EOF)) {
		statement(&parser);
	}

	endCompiler(&parser);
	return !parser.hadError;
}
