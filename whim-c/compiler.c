#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

static Chunk* currentChunk(VM* vm) {
	return &vm->compiler->function->chunk;
}

static void initCompiler(VM* vm, Compiler* compiler, FunctionType type) {
	compiler->enclosing = vm->compiler;
	compiler->function = NULL;
	compiler->type = type;

	compiler->localCount = 0;
	compiler->loopCount = 0;
	compiler->scopeDepth = 0;
	compiler->function = newFunction(vm);

	Local* local = &compiler->locals[compiler->localCount++];
	local->depth = 0;
	local->isCaptured = false;
	local->name.start = "";
	local->name.length = 0;

	vm->compiler = compiler;
}

static void errorAt(VM* vm, Token* token, const char* message) {
	if (vm->parser.panicMode) return;
	vm->parser.panicMode = true;

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
	vm->parser.hadError = true;
}

static void error(VM* vm, const char* message) {
	errorAt(vm, &vm->parser.previous, message);
}

static void errorAtCurrent(VM* vm, const char* message) {
	errorAt(vm, &vm->parser.current, message);
}

static void advance(VM* vm) {
	vm->parser.previous = vm->parser.current;

	for (;;) {
		vm->parser.current = scanToken(&vm->scanner);
		if (vm->parser.current.type != TOKEN_ERROR) break;

		errorAtCurrent(vm, vm->parser.current.start);
	}
}

static void consume(VM* vm, TokenType type, const char* message) {
	if (vm->parser.current.type == type) {
		advance(vm);
		return;
	}

	errorAtCurrent(vm, message);
}

static bool check(VM* vm, TokenType type) {
	return vm->parser.current.type == type;
}

static bool match(VM* vm, TokenType type) {
	if (!check(vm, type)) return false;
	advance(vm);
	return true;
}

static void emitByte(VM* vm, uint8_t byte) {
	writeChunk(vm, currentChunk(vm), byte, vm->parser.previous.line);
}

static void emitBytes(VM* vm, uint8_t byte1, uint8_t byte2) {
	emitByte(vm, byte1);
	emitByte(vm, byte2);
}

static int emitJump(VM* vm, uint8_t instruction) {
	emitByte(vm, instruction);
	emitByte(vm, 0xff);
	emitByte(vm, 0xff);
	return currentChunk(vm)->count - 2;
}

static void emitLoop(VM* vm, int loopStart) {
	emitByte(vm, OP_JUMP_BACK);

	int offset = currentChunk(vm)->count - loopStart + 2;
	if (offset > UINT16_MAX) error(vm, "Loop body too large.");

	emitByte(vm, (offset >> 8) & 0xff);
	emitByte(vm, offset & 0xff);
}

static void emitReturn(VM* vm) {
	emitByte(vm, OP_NIL);
	emitByte(vm, OP_RETURN);
}

static uint8_t makeConstant(VM* vm, Value value) {
	int constant = addConstant(vm, currentChunk(vm), value);
	if (constant > UINT8_MAX) {
		error(vm, "Too many constants in one chunk.");
		return 0;
	}

	return (uint8_t)constant;
}

static void emitConstant(VM* vm, Value value) {
	emitBytes(vm, OP_CONSTANT, makeConstant(vm, value));
}

static void patchJump(VM* vm, int offset) {
	// -2 to adjust for the bytecode for the jump offset itself
	int jump = currentChunk(vm)->count - offset - 2;

	if (jump > UINT16_MAX) {
		error(vm, "Too much code to jump over.");
	}

	currentChunk(vm)->code[offset] = (jump >> 8) & 0xff;
	currentChunk(vm)->code[offset + 1] = jump & 0xff;
}

static ObjFunction* endCompiler(VM* vm) {
	emitReturn(vm);
	ObjFunction* function = vm->compiler->function;

#ifdef DEBUG_PRINT_CODE
	if (!vm->parser.hadError) {
		disassembleChunk(currentChunk(vm), function->name != NULL ?
			function->name->chars : "<script>");
	}
#endif

	vm->compiler = vm->compiler->enclosing;

	return function;
}

static void beginScope(VM* vm) {
	vm->compiler->scopeDepth++;
}

static void endScope(VM* vm) {
	vm->compiler->scopeDepth--;

	while (vm->compiler->localCount > 0 &&
		vm->compiler->locals[vm->compiler->localCount - 1].depth > vm->compiler->scopeDepth) {
		if (vm->compiler->locals[vm->compiler->localCount - 1].isCaptured) {
			emitByte(vm, OP_CLOSE_UPVALUE);
		}
		else {
			emitByte(vm, OP_POP);
		}
		vm->compiler->localCount--;
	}
}

static void scopePop(VM* vm, int depth) {
	for (int i = vm->compiler->localCount - 1; i >= 0; i--) {
		if (vm->compiler->locals[i].depth < depth) return;
		emitByte(vm, OP_POP);
	}
}

static void expression(VM* vm);
static void statement(VM* vm);
static void block(VM* vm, TokenType end, const char* missingMessage);
static void declaration(VM* vm);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(VM* vm, Precedence precedence);

static uint8_t identifierConstant(VM* vm, Token* name) {
	return makeConstant(vm, OBJ_VAL(copyString(vm, name->start, name->length)));
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

static int addUpvalue(VM* vm, uint8_t index, bool isLocal) {
	Compiler* compiler = vm->compiler;
	int upvalueCount = compiler->function->upvalueCount;

	for (int i = 0; i < upvalueCount; i++) {
		Upvalue* upvalue = &compiler->upvalues[i];
		if (upvalue->index == index && upvalue->isLocal == isLocal) {
			return i;
		}
	}

	if (upvalueCount == UINT8_COUNT) {
		error(vm, "Too many closure variables in function.");
		return 0;
	}

	compiler->upvalues[upvalueCount].isLocal = isLocal;
	compiler->upvalues[upvalueCount].index = index;
	return compiler->function->upvalueCount++;
}

static int resolveUpvalue(VM* vm, Compiler* compiler, Token* identifier) {
	if (compiler->enclosing == NULL) return -1;

	int local = resolveLocal(compiler->enclosing, identifier);
	if (local != -1) {
		compiler->enclosing->locals[local].isCaptured = true;
		return addUpvalue(vm, (uint8_t)local, true);
	}

	int upvalue = resolveUpvalue(vm, compiler->enclosing, identifier);
	if (upvalue != -1) {
		return addUpvalue(vm, (uint8_t)upvalue, false);
	}

	return -1;
}

static Local* getUpvalueLocal(VM* vm, int upvalueIndex) {
	Compiler* comp = vm->compiler;
	int index = upvalueIndex;
	while (!comp->upvalues[index].isLocal) {
		index = comp->upvalues[index].index;
		comp = comp->enclosing;
	}
	return &comp->enclosing->locals[comp->upvalues[index].index];
}

static void addLocal(VM* vm, Token identifier, bool constant) {
	Compiler* compiler = vm->compiler;
	if (compiler->localCount == UINT8_COUNT) {
		error(vm, "Too many local variables in block.");
		return;
	}

	Local* local = &compiler->locals[compiler->localCount++];
	local->name = identifier;
	local->constant = constant;
	local->depth = -1;
	local->isCaptured = false;
}

static void markInitialized(VM* vm) {
	Compiler* compiler = vm->compiler;
	compiler->locals[compiler->localCount - 1].depth = compiler->scopeDepth;
}

static void declareLocal(VM* vm, Token* identifier, bool constant) {
	if (resolveLocal(vm->compiler, identifier) != -1) {
		error(vm, "A variable with this name already exists.");
	}
	if (resolveUpvalue(vm, vm->compiler, identifier) != -1) {
		error(vm, "A variable with this name already exists.");
	}

	addLocal(vm, *identifier, constant);
}

static void defineGlobal(VM* vm, uint8_t global, bool constant) {
	emitBytes(vm, constant ? OP_DEFINE_GLOBAL_CONST : OP_DEFINE_GLOBAL_VAR, global);
}

static uint8_t argumentList(VM* vm) {
	uint8_t argCount = 0;
	if (!check(vm, TOKEN_RIGHT_PAREN)) {
		do {
			expression(vm);
			if (argCount == 255) {
				error(vm, "Can't have more than 255 arguments.");
			}
			argCount++;
		} while (match(vm, TOKEN_COMMA) && !check(vm, TOKEN_RIGHT_PAREN));
	}
	consume(vm, TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
	return argCount;
}

static void and_expr(VM* vm) {
	int endJump = emitJump(vm, OP_JUMP_IF_FALSE);

	emitByte(vm, OP_POP);
	parsePrecedence(vm, PREC_AND);

	patchJump(vm, endJump);
}

static void binary(VM* vm) {
	TokenType operatorType = vm->parser.previous.type;
	ParseRule* rule = getRule(operatorType);
	parsePrecedence(vm, (Precedence)(rule->precedence + 1));

	switch (operatorType) {
	case TOKEN_BANG_EQUAL:		emitByte(vm, OP_NOT_EQUAL); break;
	case TOKEN_EQUAL_EQUAL:		emitByte(vm, OP_EQUAL); break;
	case TOKEN_GREATER:			emitByte(vm, OP_GREATER); break;
	case TOKEN_GREATER_EQUAL:	emitByte(vm, OP_GREATER_EQUAL); break;
	case TOKEN_LESS:			emitByte(vm, OP_LESS); break;
	case TOKEN_LESS_EQUAL:		emitByte(vm, OP_LESS_EQUAL); break;
	case TOKEN_PLUS:			emitByte(vm, OP_ADD); break;
	case TOKEN_MINUS:			emitByte(vm, OP_SUBTRACT); break;
	case TOKEN_STAR:			emitByte(vm, OP_MULTIPLY); break;
	case TOKEN_SLASH:			emitByte(vm, OP_DIVIDE); break;
	case TOKEN_PERCENT:			emitByte(vm, OP_MODULUS); break;
	default: return;			// unreachable
	}
}

static void call(VM* vm) {
	uint8_t argCount = argumentList(vm);
	emitBytes(vm, OP_CALL, argCount);
}

static void function(VM* vm) {
	Compiler compiler;
	initCompiler(vm, &compiler, TYPE_FUNCTION);
	if (vm->compiler->enclosing->isNamedDeclaration) {
		compiler.function->name = copyString(vm, vm->compiler->enclosing->nameStart,
			vm->compiler->enclosing->nameLength);
	}
	beginScope(vm);

	consume(vm, TOKEN_LEFT_PAREN, "Expect '(' after fn.");
	if (!check(vm, TOKEN_RIGHT_PAREN)) {
		do {
			vm->compiler->function->arity++;
			if (vm->compiler->function->arity > 255) {
				errorAtCurrent(vm, "Can't have more than 255 parameters.");
			}
			consume(vm, TOKEN_IDENTIFIER, "Expect parameter name.");
			declareLocal(vm, &vm->parser.previous, true);
			markInitialized(vm);
		} while (match(vm, TOKEN_COMMA) && !check(vm, TOKEN_RIGHT_PAREN));
	}
	consume(vm, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
	block(vm, TOKEN_FN_END, "Expect '/fn' after block.");

	ObjFunction* function = endCompiler(vm);
	emitBytes(vm, OP_CLOSURE, makeConstant(vm, OBJ_VAL(function)));

	for (int i = 0; i < function->upvalueCount; i++) {
		emitByte(vm, compiler.upvalues[i].isLocal ? 1 : 0);
		emitByte(vm, compiler.upvalues[i].index);
	}
}

static void class_expr(VM* vm) {
	if (vm->compiler->isNamedDeclaration) {
		uint8_t nameConstant = makeConstant(vm, OBJ_VAL(copyString(vm, vm->compiler->nameStart, vm->compiler->nameLength)));
		emitBytes(vm, OP_CLASS, nameConstant);
	}
	else {
		emitByte(vm, OP_ANON_CLASS);
	}

	consume(vm, TOKEN_CLASS_END, "Expect '/class' after block.");
}

static void literal(VM* vm) {
	switch (vm->parser.previous.type) {
	case TOKEN_FALSE:	emitByte(vm, OP_FALSE); break;
	case TOKEN_NIL:		emitByte(vm, OP_NIL); break;
	case TOKEN_TRUE:	emitByte(vm, OP_TRUE); break;
	default: return;	// unreachable
	}
}

static void grouping(VM* vm) {
	expression(vm);
	consume(vm, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(VM* vm) {
	double value = strtod(vm->parser.previous.start, NULL);
	emitConstant(vm, NUMBER_VAL(value));
}

static void or_expr(VM* vm) {
	int endJump = emitJump(vm, OP_JUMP_IF_TRUE);

	emitByte(vm, OP_POP);
	parsePrecedence(vm, PREC_OR);

	patchJump(vm, endJump);
}

static void string(VM* vm) {
	emitConstant(vm, OBJ_VAL(copyEscapeString(vm,
		vm->parser.previous.start + 1,
		vm->parser.previous.length - 2)));
}

static void namedVariable(VM* vm, Token name) {
	uint8_t getOp;
	int arg = resolveLocal(vm->compiler, &name);
	if (arg != -1) {
		getOp = OP_GET_LOCAL;
	}
	else if ((arg = resolveUpvalue(vm, vm->compiler, &name)) != -1) {
		getOp = OP_GET_UPVALUE;
	}
	else {
		arg = identifierConstant(vm, &name);
		getOp = OP_GET_GLOBAL;
	}
	emitBytes(vm, getOp, (uint8_t)arg);
}

static void variable(VM* vm) {
	namedVariable(vm, vm->parser.previous);
}

static void unary(VM* vm) {
	TokenType operatorType = vm->parser.previous.type;

	// compile the operand
	parsePrecedence(vm, PREC_UNARY);

	// emit the operator instruction
	switch (operatorType)
	{
	case TOKEN_BANG:	emitByte(vm, OP_NOT); break;
	case TOKEN_MINUS:	emitByte(vm, OP_NEGATE); break;
	default: return;	// unreachable
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
	[TOKEN_CLASS] = {			class_expr,	NULL,		PREC_NONE},
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

static void parsePrecedenceFromPrevious(VM* vm, Precedence precedence) {
	ParseFn prefixRule = getRule(vm->parser.previous.type)->prefix;
	if (prefixRule == NULL) {
		error(vm, "Expect expression.");
		return;
	}

	prefixRule(vm);

	while (precedence <= getRule(vm->parser.current.type)->precedence) {
		advance(vm);
		ParseFn infixRule = getRule(vm->parser.previous.type)->infix;
		infixRule(vm);
	}
}

static void parsePrecedence(VM* vm, Precedence precedence) {
	advance(vm);
	parsePrecedenceFromPrevious(vm, precedence);
}

static ParseRule* getRule(TokenType type) {
	return &rules[type];
}

static void expression(VM* vm) {
	parsePrecedence(vm, PREC_OR);
}

static void expressionFromPrevious(VM* vm) {
	parsePrecedenceFromPrevious(vm, PREC_OR);
}

static void block(VM* vm, TokenType end, const char* missingMessage) {
	while (!check(vm, end) && !check(vm, TOKEN_EOF)) {
		declaration(vm);
	}

	consume(vm, end, missingMessage);
}

static void breakStatement(VM* vm) {
	if (vm->compiler->loopCount == 0) {
		error(vm, "Cannot break, not in a loop.");
		return;
	}

	Loop* loop = &vm->compiler->loops[vm->compiler->loopCount - 1];
	scopePop(vm, loop->depth);
	patchJump(vm, loop->exit);
	loop->exit = emitJump(vm, OP_JUMP);
}

static void continueStatement(VM* vm) {
	if (vm->compiler->loopCount == 0) {
		error(vm, "Cannot continue, not in a loop.");
		return;
	}

	Loop* loop = &vm->compiler->loops[vm->compiler->loopCount - 1];
	scopePop(vm, loop->depth);
	emitLoop(vm, loop->start);
}

static void expressionStatement(VM* vm) {
	// parse one primary expression and check for declaration or assignment
	// if not, then parse as a normal expression statement
	if (match(vm, TOKEN_IDENTIFIER)) {
		switch (vm->parser.current.type) {
		case TOKEN_COLON_COLON:
		case TOKEN_COLON_EQUAL: {
			// declaration

			bool constant = vm->parser.current.type == TOKEN_COLON_COLON;

			vm->compiler->isNamedDeclaration = true;
			vm->compiler->nameStart = vm->parser.previous.start;
			vm->compiler->nameLength = vm->parser.previous.length;

			if (vm->compiler->scopeDepth > 0) {
				declareLocal(vm, &vm->parser.previous, constant);
				advance(vm);	// accept :: :=

				// mark initialized if it's a function so it can recursively call itself

				// TODO - class as well?
				if (vm->parser.current.type == TOKEN_FN) {
					markInitialized(vm);
				}

				expression(vm);
				markInitialized(vm);
			}
			else {
				uint8_t arg = identifierConstant(vm, &vm->parser.previous);
				advance(vm);	// accept :: :=
				expression(vm);
				defineGlobal(vm, arg, constant);
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

			vm->compiler->isNamedDeclaration = true;
			vm->compiler->nameStart = vm->parser.previous.start;
			vm->compiler->nameLength = vm->parser.previous.length;

			int arg = resolveLocal(vm->compiler, &vm->parser.previous);

			uint8_t op;
			if (arg != -1) {
				// local
				if (vm->compiler->locals[arg].constant) {
					error(vm, "Local is constant.");
				}

				op = OP_SET_LOCAL;
				switch (vm->parser.current.type) {
				case TOKEN_PLUS_EQUAL:		op = OP_ADD_SET_LOCAL; break;
				case TOKEN_MINUS_EQUAL:		op = OP_SUBTRACT_SET_LOCAL; break;
				case TOKEN_STAR_EQUAL:		op = OP_MULTIPLY_SET_LOCAL; break;
				case TOKEN_SLASH_EQUAL:		op = OP_DIVIDE_SET_LOCAL; break;
				case TOKEN_PERCENT_EQUAL:	op = OP_MODULUS_SET_LOCAL; break;
				}
			}
			else if ((arg = resolveUpvalue(vm, vm->compiler, &vm->parser.previous)) != -1) {
				// upvalue
				Local* local = getUpvalueLocal(vm, arg);
				if (local->constant) {
					error(vm, "Local is constant.");
				}

				op = OP_SET_UPVALUE;
				switch (vm->parser.current.type) {
				case TOKEN_PLUS_EQUAL:		op = OP_ADD_SET_UPVALUE; break;
				case TOKEN_MINUS_EQUAL:		op = OP_SUBTRACT_SET_UPVALUE; break;
				case TOKEN_STAR_EQUAL:		op = OP_MULTIPLY_SET_UPVALUE; break;
				case TOKEN_SLASH_EQUAL:		op = OP_DIVIDE_SET_UPVALUE; break;
				case TOKEN_PERCENT_EQUAL:	op = OP_MODULUS_SET_UPVALUE; break;
				}
			}
			else {
				// global
				arg = identifierConstant(vm, &vm->parser.previous);

				op = OP_SET_GLOBAL;
				switch (vm->parser.current.type) {
				case TOKEN_PLUS_EQUAL:		op = OP_ADD_SET_GLOBAL; break;
				case TOKEN_MINUS_EQUAL:		op = OP_SUBTRACT_SET_GLOBAL; break;
				case TOKEN_STAR_EQUAL:		op = OP_MULTIPLY_SET_GLOBAL; break;
				case TOKEN_SLASH_EQUAL:		op = OP_DIVIDE_SET_GLOBAL; break;
				case TOKEN_PERCENT_EQUAL:	op = OP_MODULUS_SET_GLOBAL; break;
				}
			}

			advance(vm);	// accept = += -= *= /= %=
			expression(vm);
			emitBytes(vm, op, (uint8_t)arg);
			break;
		}
		default:
			// not an assignment, continue parsing the expression
			expressionFromPrevious(vm);
			emitByte(vm, OP_POP);
			break;
		}
	}
	else {
		expression(vm);
		emitByte(vm, OP_POP);
	}
}

static void forStatement(VM* vm) {
	if (vm->compiler->loopCount == MAX_LOOP) {
		error(vm, "Too many nested loops.");
		return;
	}

	beginScope(vm);

	Loop* loop = &vm->compiler->loops[vm->compiler->loopCount++];
	loop->start = currentChunk(vm)->count;
	loop->depth = vm->compiler->scopeDepth;

	if (match(vm, TOKEN_IDENTIFIER)) {
		if (match(vm, TOKEN_IN)) {
			// todo - iterator
		}
		else {
			// not an iterator, continue parsing the expression
			expressionFromPrevious(vm);
		}
	}
	else {
		expression(vm);
	}

	loop->exit = emitJump(vm, OP_JUMP_IF_FALSE_POP);

	while (vm->parser.current.type != TOKEN_FOR_END && vm->parser.current.type != TOKEN_EOF) {
		statement(vm);
	}

	endScope(vm);

	emitLoop(vm, loop->start);

	patchJump(vm, loop->exit);

	consume(vm, TOKEN_FOR_END, "Expect '/for' after block.");

	vm->compiler->loopCount--;
}

static void ifStatement(VM* vm) {
	expression(vm);

	int thenJump = emitJump(vm, OP_JUMP_IF_FALSE_POP);

	beginScope(vm);
	while (vm->parser.current.type != TOKEN_IF_END &&
		vm->parser.current.type != TOKEN_ELIF &&
		vm->parser.current.type != TOKEN_ELSE &&
		vm->parser.current.type != TOKEN_EOF) {

		statement(vm);
	}
	endScope(vm);

	int elseJump = emitJump(vm, OP_JUMP);

	patchJump(vm, thenJump);

	while (match(vm, TOKEN_ELIF)) {
		expression(vm);

		thenJump = emitJump(vm, OP_JUMP_IF_FALSE_POP);

		beginScope(vm);
		while (vm->parser.current.type != TOKEN_IF_END &&
			vm->parser.current.type != TOKEN_ELIF &&
			vm->parser.current.type != TOKEN_ELSE &&
			vm->parser.current.type != TOKEN_EOF) {

			statement(vm);
		}
		endScope(vm);

		patchJump(vm, elseJump);
		elseJump = emitJump(vm, OP_JUMP);
		patchJump(vm, thenJump);
	}

	if (match(vm, TOKEN_ELSE)) {
		beginScope(vm);
		while (vm->parser.current.type != TOKEN_IF_END &&
			vm->parser.current.type != TOKEN_EOF) {

			statement(vm);
		}
		endScope(vm);
	}

	patchJump(vm, elseJump);

	consume(vm, TOKEN_IF_END, "Expect '/if' after block.");
}

static void returnStatement(VM* vm) {
	expression(vm);
	emitByte(vm, OP_RETURN);
}

static void synchronize(VM* vm) {
	vm->parser.panicMode = false;

	while (vm->parser.current.type != TOKEN_EOF) {
		switch (vm->parser.previous.type) {
		case TOKEN_CLASS_END:
		case TOKEN_DO_END:
		case TOKEN_FN_END:
		case TOKEN_FOR_END:
		case TOKEN_IF_END:
		case TOKEN_SEMICOLON:
		case TOKEN_TRY_END:
			return;
		}

		switch (vm->parser.current.type) {
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

		advance(vm);
	}
}

static void declaration(VM* vm) {
	statement(vm);

	if (vm->parser.panicMode) synchronize(vm);
}

static void statement(VM* vm) {
	vm->compiler->isNamedDeclaration = false;

	if (match(vm, TOKEN_SEMICOLON)) {
		// empty statement
	}
	else if (match(vm, TOKEN_BREAK)) {
		breakStatement(vm);
	}
	else if (match(vm, TOKEN_CONTINUE)) {
		continueStatement(vm);
	}
	else if (match(vm, TOKEN_DO)) {
		beginScope(vm);
		block(vm, TOKEN_DO_END, "Expect '/do' after block.");
		endScope(vm);
	}
	else if (match(vm, TOKEN_FOR)) {
		forStatement(vm);
	}
	else if (match(vm, TOKEN_IF)) {
		ifStatement(vm);
	}
	else if (match(vm, TOKEN_RETURN)) {
		returnStatement(vm);
	}
	else {
		expressionStatement(vm);
	}
}

ObjFunction* compile(VM* vm, const char* source) {
	initScanner(&vm->scanner, source);

	vm->parser.hadError = false;
	vm->parser.panicMode = false;

	Compiler compiler;
	initCompiler(vm, &compiler, TYPE_SCRIPT);

	advance(vm);

	while (!match(vm, TOKEN_EOF)) {
		declaration(vm);
	}

	ObjFunction* function = endCompiler(vm);
	return vm->parser.hadError ? NULL : function;
}

void markCompilerRoots(VM* vm) {
	Compiler* comp = vm->compiler;
	while (comp != NULL) {
		markObject(vm, (Obj*)comp->function);
		comp = comp->enclosing;
	}
}
