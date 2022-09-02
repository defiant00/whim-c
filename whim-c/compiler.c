#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

static Chunk* currentChunk(Compiler* compiler) {
	return &compiler->function->chunk;
}

static void initCompiler(Compiler* compiler, FunctionType type, VM* vm, Parser* parser) {
	compiler->vm = vm;

	compiler->function = NULL;
	compiler->type = type;

	compiler->localCount = 0;
	compiler->loopCount = 0;
	compiler->scopeDepth = 0;
	compiler->function = newFunction(vm);

	compiler->parser = parser;

	Local* local = &compiler->locals[compiler->localCount++];
	local->depth = 0;
	local->name.start = "";
	local->name.length = 0;
}

static void errorAt(Compiler* compiler, Token* token, const char* message) {
	if (compiler->parser->panicMode) return;
	compiler->parser->panicMode = true;

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
	compiler->parser->hadError = true;
}

static void error(Compiler* compiler, const char* message) {
	errorAt(compiler, &compiler->parser->previous, message);
}

static void errorAtCurrent(Compiler* compiler, const char* message) {
	errorAt(compiler, &compiler->parser->current, message);
}

static void advance(Compiler* compiler) {
	compiler->parser->previous = compiler->parser->current;

	for (;;) {
		compiler->parser->current = scanToken(compiler->parser->scanner);
		if (compiler->parser->current.type != TOKEN_ERROR) break;

		errorAtCurrent(compiler, compiler->parser->current.start);
	}
}

static void consume(Compiler* compiler, TokenType type, const char* message) {
	if (compiler->parser->current.type == type) {
		advance(compiler);
		return;
	}

	errorAtCurrent(compiler, message);
}

static bool check(Compiler* compiler, TokenType type) {
	return compiler->parser->current.type == type;
}

static bool match(Compiler* compiler, TokenType type) {
	if (!check(compiler, type)) return false;
	advance(compiler);
	return true;
}

static void emitByte(Compiler* compiler, uint8_t byte) {
	writeChunk(currentChunk(compiler), byte, compiler->parser->previous.line);
}

static void emitBytes(Compiler* compiler, uint8_t byte1, uint8_t byte2) {
	emitByte(compiler, byte1);
	emitByte(compiler, byte2);
}

static int emitJump(Compiler* compiler, uint8_t instruction) {
	emitByte(compiler, instruction);
	emitByte(compiler, 0xff);
	emitByte(compiler, 0xff);
	return currentChunk(compiler)->count - 2;
}

static void emitLoop(Compiler* compiler, int loopStart) {
	emitByte(compiler, OP_JUMP_BACK);

	int offset = currentChunk(compiler)->count - loopStart + 2;
	if (offset > UINT16_MAX) error(compiler, "Loop body too large.");

	emitByte(compiler, (offset >> 8) & 0xff);
	emitByte(compiler, offset & 0xff);
}

static void emitReturn(Compiler* compiler) {
	emitByte(compiler, OP_NIL);
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

static void patchJump(Compiler* compiler, int offset) {
	// -2 to adjust for the bytecode for the jump offset itself
	int jump = currentChunk(compiler)->count - offset - 2;

	if (jump > UINT16_MAX) {
		error(compiler, "Too much code to jump over.");
	}

	currentChunk(compiler)->code[offset] = (jump >> 8) & 0xff;
	currentChunk(compiler)->code[offset + 1] = jump & 0xff;
}

static ObjFunction* endCompiler(Compiler* compiler) {
	emitReturn(compiler);
	ObjFunction* function = compiler->function;

#ifdef DEBUG_PRINT_CODE
	if (!compiler->parser->hadError) {
		disassembleChunk(currentChunk(compiler), function->name != NULL ?
			function->name->chars : "<script>");
	}
#endif

	return function;
}

static void beginScope(Compiler* compiler) {
	compiler->scopeDepth++;
}

static void endScope(Compiler* compiler) {
	compiler->scopeDepth--;

	while (compiler->localCount > 0 &&
		compiler->locals[compiler->localCount - 1].depth > compiler->scopeDepth) {
		emitByte(compiler, OP_POP);
		compiler->localCount--;
	}
}

static void scopePop(Compiler* compiler, int depth) {
	for (int i = compiler->localCount - 1; i >= 0; i--) {
		if (compiler->locals[i].depth < depth) return;
		emitByte(compiler, OP_POP);
	}
}

static void expression(Compiler* compiler);
static void statement(Compiler* compiler);
static void block(Compiler* compiler, TokenType end, const char* missingMessage);
static void declaration(Compiler* compiler);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Compiler* compiler, Precedence precedence);

static uint8_t identifierConstant(Compiler* compiler, Token* name) {
	return makeConstant(compiler, OBJ_VAL(copyString(compiler->vm, name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
	if (a->length != b->length) return false;
	return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* identifier) {
	for (int i = compiler->localCount - 1; i >= 0; i--) {
		Local* local = &compiler->locals[i];
		if (identifiersEqual(identifier, &local->name)) {
			return local->depth == -1 ? -1 : i;
		}
	}
	return -1;
}

static void addLocal(Compiler* compiler, Token identifier, bool constant) {
	if (compiler->localCount == UINT8_COUNT) {
		error(compiler, "Too many local variables in block.");
		return;
	}

	Local* local = &compiler->locals[compiler->localCount++];
	local->name = identifier;
	local->constant = constant;
	local->depth = -1;
}

static void markInitialized(Compiler* compiler) {
	compiler->locals[compiler->localCount - 1].depth = compiler->scopeDepth;
}

static void declareLocal(Compiler* compiler, Token* identifier, bool constant) {
	int arg = resolveLocal(compiler, identifier);
	if (arg != -1) {
		error(compiler, "A variable with this name already exists.");
	}

	addLocal(compiler, *identifier, constant);
}

static void defineGlobal(Compiler* compiler, uint8_t global, bool constant) {
	emitBytes(compiler, constant ? OP_DEFINE_GLOBAL_CONST : OP_DEFINE_GLOBAL_VAR, global);
}

static uint8_t argumentList(Compiler* compiler) {
	uint8_t argCount = 0;
	if (!check(compiler, TOKEN_RIGHT_PAREN)) {
		do {
			expression(compiler);
			if (argCount == 255) {
				error(compiler, "Can't have more than 255 arguments.");
			}
			argCount++;
		} while (match(compiler, TOKEN_COMMA) && !check(compiler, TOKEN_RIGHT_PAREN));
	}
	consume(compiler, TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
	return argCount;
}

static void and_expr(Compiler* compiler) {
	int endJump = emitJump(compiler, OP_JUMP_IF_FALSE);

	emitByte(compiler, OP_POP);
	parsePrecedence(compiler, PREC_AND);

	patchJump(compiler, endJump);
}

static void binary(Compiler* compiler) {
	TokenType operatorType = compiler->parser->previous.type;
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

static void call(Compiler* compiler) {
	uint8_t argCount = argumentList(compiler);
	emitBytes(compiler, OP_CALL, argCount);
}

static void function(Compiler* compiler) {
	Compiler comp;
	initCompiler(&comp, TYPE_FUNCTION, compiler->vm, compiler->parser);
	comp.enclosing = compiler;
	if (compiler->isNamedDeclaration) {
		comp.function->name = copyString(compiler->vm, compiler->nameStart, compiler->nameLength);
	}
	beginScope(&comp);

	consume(&comp, TOKEN_LEFT_PAREN, "Expect '(' after fn.");
	if (!check(&comp, TOKEN_RIGHT_PAREN)) {
		do {
			comp.function->arity++;
			if (comp.function->arity > 255) {
				errorAtCurrent(&comp, "Can't have more than 255 parameters.");
			}
			consume(&comp, TOKEN_IDENTIFIER, "Expect parameter name.");
			declareLocal(&comp, &comp.parser->previous, true);
			markInitialized(&comp);
		} while (match(&comp, TOKEN_COMMA) && !check(&comp, TOKEN_RIGHT_PAREN));
	}
	consume(&comp, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
	block(&comp, TOKEN_FN_END, "Expect '/fn' after block.");

	ObjFunction* function = endCompiler(&comp);
	emitBytes(compiler, OP_CONSTANT, makeConstant(compiler, OBJ_VAL(function)));
}

static void literal(Compiler* compiler) {
	switch (compiler->parser->previous.type) {
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
	double value = strtod(compiler->parser->previous.start, NULL);
	emitConstant(compiler, NUMBER_VAL(value));
}

static void or_expr(Compiler* compiler) {
	int endJump = emitJump(compiler, OP_JUMP_IF_TRUE);

	emitByte(compiler, OP_POP);
	parsePrecedence(compiler, PREC_OR);

	patchJump(compiler, endJump);
}

static void string(Compiler* compiler) {
	emitConstant(compiler, OBJ_VAL(copyEscapeString(compiler->vm,
		compiler->parser->previous.start + 1,
		compiler->parser->previous.length - 2)));
}

static void namedVariable(Compiler* compiler, Token name) {
	uint8_t getOp;
	int arg = resolveLocal(compiler, &name);
	if (arg != -1) {
		getOp = OP_GET_LOCAL;
	}
	else {
		arg = identifierConstant(compiler, &name);
		getOp = OP_GET_GLOBAL;
	}
	emitBytes(compiler, getOp, (uint8_t)arg);
}

static void variable(Compiler* compiler) {
	namedVariable(compiler, compiler->parser->previous);
}

static void unary(Compiler* compiler) {
	TokenType operatorType = compiler->parser->previous.type;

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
	[TOKEN_LEFT_PAREN] = {		grouping,	call,		PREC_CALL},
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
	[TOKEN_AND] = {				NULL,		and_expr,	PREC_AND},
	[TOKEN_BREAK] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_CATCH] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_CLASS] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_CONTINUE] = {		NULL,		NULL,		PREC_NONE},
	[TOKEN_DO] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_ELSE] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FALSE] = {			literal,	NULL,		PREC_NONE},
	[TOKEN_FINALLY] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_FN] = {				function,	NULL,		PREC_NONE},
	[TOKEN_FOR] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_FROM] = {			NULL,		NULL,		PREC_NONE},
	[TOKEN_IF] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_IN] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_IS] = {				NULL,		NULL,		PREC_NONE},
	[TOKEN_NIL] = {				literal,	NULL,		PREC_NONE},
	[TOKEN_OR] = {				NULL,		or_expr,	PREC_OR},
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
	ParseFn prefixRule = getRule(compiler->parser->previous.type)->prefix;
	if (prefixRule == NULL) {
		error(compiler, "Expect expression.");
		return;
	}

	prefixRule(compiler);

	while (precedence <= getRule(compiler->parser->current.type)->precedence) {
		advance(compiler);
		ParseFn infixRule = getRule(compiler->parser->previous.type)->infix;
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

static void block(Compiler* compiler, TokenType end, const char* missingMessage) {
	while (!check(compiler, end) && !check(compiler, TOKEN_EOF)) {
		declaration(compiler);
	}

	consume(compiler, end, missingMessage);
}

static void breakStatement(Compiler* compiler) {
	if (compiler->loopCount == 0) {
		error(compiler, "Cannot break, not in a loop.");
		return;
	}

	Loop* loop = &compiler->loops[compiler->loopCount - 1];
	scopePop(compiler, loop->depth);
	patchJump(compiler, loop->exit);
	loop->exit = emitJump(compiler, OP_JUMP);
}

static void continueStatement(Compiler* compiler) {
	if (compiler->loopCount == 0) {
		error(compiler, "Cannot continue, not in a loop.");
		return;
	}

	Loop* loop = &compiler->loops[compiler->loopCount - 1];
	scopePop(compiler, loop->depth);
	emitLoop(compiler, loop->start);
}

static void expressionStatement(Compiler* compiler) {
	// parse one primary expression and check for declaration or assignment
	// if not, then parse as a normal expression statement
	if (match(compiler, TOKEN_IDENTIFIER)) {
		switch (compiler->parser->current.type) {
		case TOKEN_COLON_COLON:
		case TOKEN_COLON_EQUAL: {
			bool constant = compiler->parser->current.type == TOKEN_COLON_COLON;
			compiler->isNamedDeclaration = true;
			compiler->nameStart = compiler->parser->previous.start;
			compiler->nameLength = compiler->parser->previous.length;

			if (compiler->scopeDepth > 0) {
				declareLocal(compiler, &compiler->parser->previous, constant);
				advance(compiler);	// accept :: :=
				expression(compiler);
				markInitialized(compiler);
			}
			else {
				uint8_t arg = identifierConstant(compiler, &compiler->parser->previous);
				advance(compiler);	// accept :: :=
				expression(compiler);
				defineGlobal(compiler, arg, constant);
			}

			break;
		}
		case TOKEN_EQUAL:
		case TOKEN_PLUS_EQUAL:
		case TOKEN_MINUS_EQUAL:
		case TOKEN_STAR_EQUAL:
		case TOKEN_SLASH_EQUAL:
		case TOKEN_PERCENT_EQUAL: {
			// assignment
			int arg = resolveLocal(compiler, &compiler->parser->previous);
			uint8_t op;
			if (arg == -1) {
				// global
				arg = identifierConstant(compiler, &compiler->parser->previous);

				op = OP_SET_GLOBAL;
				switch (compiler->parser->current.type) {
				case TOKEN_PLUS_EQUAL: op = OP_ADD_SET_GLOBAL; break;
				case TOKEN_MINUS_EQUAL: op = OP_SUBTRACT_SET_GLOBAL; break;
				case TOKEN_STAR_EQUAL: op = OP_MULTIPLY_SET_GLOBAL; break;
				case TOKEN_SLASH_EQUAL: op = OP_DIVIDE_SET_GLOBAL; break;
				case TOKEN_PERCENT_EQUAL: op = OP_MODULUS_SET_GLOBAL; break;
				}
			}
			else {
				// local
				if (compiler->locals[arg].constant) {
					error(compiler, "Local is constant.");
				}

				op = OP_SET_LOCAL;
				switch (compiler->parser->current.type) {
				case TOKEN_PLUS_EQUAL: op = OP_ADD_SET_LOCAL; break;
				case TOKEN_MINUS_EQUAL: op = OP_SUBTRACT_SET_LOCAL; break;
				case TOKEN_STAR_EQUAL: op = OP_MULTIPLY_SET_LOCAL; break;
				case TOKEN_SLASH_EQUAL: op = OP_DIVIDE_SET_LOCAL; break;
				case TOKEN_PERCENT_EQUAL: op = OP_MODULUS_SET_LOCAL; break;
				}
			}

			advance(compiler);	// accept = += -= *= /= %=
			expression(compiler);
			emitBytes(compiler, op, (uint8_t)arg);
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

static void forStatement(Compiler* compiler) {
	if (compiler->loopCount == MAX_LOOP) {
		error(compiler, "Too many nested loops.");
		return;
	}

	beginScope(compiler);

	Loop* loop = &compiler->loops[compiler->loopCount++];
	loop->start = currentChunk(compiler)->count;
	loop->depth = compiler->scopeDepth;

	if (match(compiler, TOKEN_IDENTIFIER)) {
		if (match(compiler, TOKEN_IN)) {
			// todo - iterator
		}
		else {
			// not an iterator, continue parsing the expression
			expressionFromPrevious(compiler);
		}
	}
	else {
		expression(compiler);
	}

	loop->exit = emitJump(compiler, OP_JUMP_IF_FALSE_POP);

	while (compiler->parser->current.type != TOKEN_FOR_END &&
		compiler->parser->current.type != TOKEN_EOF) {

		statement(compiler);
	}

	endScope(compiler);

	emitLoop(compiler, loop->start);

	patchJump(compiler, loop->exit);

	consume(compiler, TOKEN_FOR_END, "Expect '/for' after block.");

	compiler->loopCount--;
}

static void ifStatement(Compiler* compiler) {
	expression(compiler);

	int thenJump = emitJump(compiler, OP_JUMP_IF_FALSE_POP);

	beginScope(compiler);
	while (compiler->parser->current.type != TOKEN_IF_END &&
		compiler->parser->current.type != TOKEN_ELIF &&
		compiler->parser->current.type != TOKEN_ELSE &&
		compiler->parser->current.type != TOKEN_EOF) {

		statement(compiler);
	}
	endScope(compiler);

	int elseJump = emitJump(compiler, OP_JUMP);

	patchJump(compiler, thenJump);

	while (match(compiler, TOKEN_ELIF)) {
		expression(compiler);

		thenJump = emitJump(compiler, OP_JUMP_IF_FALSE_POP);

		beginScope(compiler);
		while (compiler->parser->current.type != TOKEN_IF_END &&
			compiler->parser->current.type != TOKEN_ELIF &&
			compiler->parser->current.type != TOKEN_ELSE &&
			compiler->parser->current.type != TOKEN_EOF) {

			statement(compiler);
		}
		endScope(compiler);

		patchJump(compiler, elseJump);
		elseJump = emitJump(compiler, OP_JUMP);
		patchJump(compiler, thenJump);
	}

	if (match(compiler, TOKEN_ELSE)) {
		beginScope(compiler);
		while (compiler->parser->current.type != TOKEN_IF_END &&
			compiler->parser->current.type != TOKEN_EOF) {

			statement(compiler);
		}
		endScope(compiler);
	}

	patchJump(compiler, elseJump);

	consume(compiler, TOKEN_IF_END, "Expect '/if' after block.");
}

static void returnStatement(Compiler* compiler) {
	expression(compiler);
	emitByte(compiler, OP_RETURN);
}

static void synchronize(Compiler* compiler) {
	compiler->parser->panicMode = false;

	while (compiler->parser->current.type != TOKEN_EOF) {
		switch (compiler->parser->previous.type) {
		case TOKEN_CLASS_END:
		case TOKEN_DO_END:
		case TOKEN_FN_END:
		case TOKEN_FOR_END:
		case TOKEN_IF_END:
		case TOKEN_SEMICOLON:
		case TOKEN_TRY_END:
			return;
		}

		switch (compiler->parser->current.type) {
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

static void declaration(Compiler* compiler) {
	statement(compiler);

	if (compiler->parser->panicMode) synchronize(compiler);
}

static void statement(Compiler* compiler) {
	compiler->isNamedDeclaration = false;

	if (match(compiler, TOKEN_SEMICOLON)) {
		// empty statement
	}
	else if (match(compiler, TOKEN_BREAK)) {
		breakStatement(compiler);
	}
	else if (match(compiler, TOKEN_CONTINUE)) {
		continueStatement(compiler);
	}
	else if (match(compiler, TOKEN_DO)) {
		beginScope(compiler);
		block(compiler, TOKEN_DO_END, "Expect '/do' after block.");
		endScope(compiler);
	}
	else if (match(compiler, TOKEN_FOR)) {
		forStatement(compiler);
	}
	else if (match(compiler, TOKEN_IF)) {
		ifStatement(compiler);
	}
	else if (match(compiler, TOKEN_RETURN)) {
		returnStatement(compiler);
	}
	else {
		expressionStatement(compiler);
	}
}

ObjFunction* compile(VM* vm, const char* source) {
	Scanner scanner;
	initScanner(&scanner, source);

	Parser parser;
	parser.hadError = false;
	parser.panicMode = false;
	parser.scanner = &scanner;

	Compiler compiler;
	initCompiler(&compiler, TYPE_SCRIPT, vm, &parser);

	advance(&compiler);

	while (!match(&compiler, TOKEN_EOF)) {
		declaration(&compiler);
	}

	ObjFunction* function = endCompiler(&compiler);
	return compiler.parser->hadError ? NULL : function;
}
