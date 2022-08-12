#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

void initScanner(Scanner* scanner, const char* source) {
	scanner->start = source;
	scanner->current = source;
	scanner->line = 1;
}

static bool isAtEnd(Scanner* scanner) {
	return *scanner->current == '\0';
}

static char advance(Scanner* scanner) {
	scanner->current++;
	return scanner->current[-1];
}

static Token makeToken(Scanner* scanner, TokenType type) {
	Token token;
	token.type = type;
	token.start = scanner->start;
	token.length = (int)(scanner->current - scanner->start);
	token.line = scanner->line;
	return token;
}

static Token errorToken(Scanner* scanner, const char* message) {
	Token token;
	token.type = TOKEN_ERROR;
	token.start = message;
	token.length = (int)strlen(message);
	token.line = scanner->line;
	return token;
}

Token scanToken(Scanner* scanner) {
	scanner->start = scanner->current;

	if (isAtEnd(scanner)) return makeToken(scanner, TOKEN_EOF);

	char c = advance(scanner);

	switch (c) {
	case '(': return makeToken(scanner, TOKEN_LEFT_PAREN);
	case ')': return makeToken(scanner, TOKEN_RIGHT_PAREN);
	case '[': return makeToken(scanner, TOKEN_LEFT_BRACKET);
	case ']': return makeToken(scanner, TOKEN_RIGHT_BRACKET);
	case ',': return makeToken(scanner, TOKEN_COMMA);
	case '.': return makeToken(scanner, TOKEN_DOT);
	case ';': return makeToken(scanner, TOKEN_SEMICOLON);
	}

	return errorToken(scanner, "Unexpected character.");
}
