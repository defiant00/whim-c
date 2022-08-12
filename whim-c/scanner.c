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

static char peek(Scanner* scanner) {
	return *scanner->current;
}

static char peekNext(Scanner* scanner) {
	if (isAtEnd(scanner)) return '\0';
	return scanner->current[1];
}

static bool match(Scanner* scanner, char expected) {
	if (isAtEnd(scanner)) return false;
	if (*scanner->current != expected) return false;
	scanner->current++;
	return true;
}

static void resetLength(Scanner* scanner) {
	scanner->start = scanner->current;
}

static bool isDigit(char c) {
	return c >= '0' && c <= '9';
}

static bool isAlpha(char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static TokenType checkKeyword(Scanner* scanner, int start, int length, const char* rest, TokenType type) {
	if (scanner->current - scanner->start == start + length && memcmp(scanner->start + start, rest, length) == 0) {
		return type;
	}
	return TOKEN_IDENTIFIER;
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

static void skipBlockComment(Scanner* scanner) {
	int depth = 1;

	while (depth > 0 && !isAtEnd(scanner)) {
		switch (advance(scanner)) {
		case '/':
			if (peek(scanner) == '*') {
				advance(scanner);
				depth += 1;
			}
			break;
		case '*':
			if (peek(scanner) == '/') {
				advance(scanner);
				depth -= 1;
			}
			break;
		}
	}

	resetLength(scanner);
}

static TokenType identifierType(Scanner* scanner) {
	switch (scanner->start[0]) {
	case 'a': return checkKeyword(scanner, 1, 2, "nd", TOKEN_AND);
	case 'b': return checkKeyword(scanner, 1, 3, "ase", TOKEN_BASE);
	case 'c': return checkKeyword(scanner, 1, 4, "lass", TOKEN_CLASS);
	case 'e': return checkKeyword(scanner, 1, 3, "lse", TOKEN_ELSE);
	case 'f':
		if (scanner->current - scanner->start > 1) {
			switch (scanner->start[1])
			{
			case 'a': return checkKeyword(scanner, 2, 3, "lse", TOKEN_FALSE);
			case 'n': if (scanner->current - scanner->start == 2) return TOKEN_FN;
			case 'o': return checkKeyword(scanner, 2, 1, "r", TOKEN_FOR);
			case 'r': return checkKeyword(scanner, 2, 2, "om", TOKEN_FROM);
			}
		}
		break;
	case 'i':
		if (scanner->current - scanner->start == 2) {
			switch (scanner->start[1]) {
			case 'f': return TOKEN_IF;
			case 's': return TOKEN_IS;
			}
		}
		break;
	case 'n': return checkKeyword(scanner, 1, 2, "il", TOKEN_NIL);
	case 'o': return checkKeyword(scanner, 1, 1, "r", TOKEN_OR);
	case 'r': return checkKeyword(scanner, 1, 5, "eturn", TOKEN_RETURN);
	case 't': return checkKeyword(scanner, 1, 3, "rue", TOKEN_TRUE);
	}
	return TOKEN_IDENTIFIER;
}

static Token identifier(Scanner* scanner) {
	while (isAlpha(peek(scanner)) || isDigit(peek(scanner))) advance(scanner);
	return makeToken(scanner, identifierType(scanner));
}

static Token number(Scanner* scanner) {
	while (isDigit(peek(scanner))) advance(scanner);

	if (peek(scanner) == '.' && isDigit(peekNext(scanner))) {
		// accept the .
		advance(scanner);

		while (isDigit(peek(scanner))) advance(scanner);
	}

	return makeToken(scanner, TOKEN_NUMBER);
}

static Token string(Scanner* scanner, char first) {
	while (peek(scanner) != first && !isAtEnd(scanner)) {
		if (peek(scanner) == '\n') scanner->line++;
		if (peek(scanner) == '\\' && peekNext(scanner) != '\0' && peekNext(scanner) != '\n') advance(scanner);
		advance(scanner);
	}

	if (isAtEnd(scanner)) return errorToken(scanner, "Unterminated string.");

	// closing quote
	advance(scanner);
	return makeToken(scanner, TOKEN_STRING);
}

Token scanToken(Scanner* scanner) {
	resetLength(scanner);

	while (!isAtEnd(scanner)) {
		char c = advance(scanner);

		if (isAlpha(c)) return identifier(scanner);
		if (isDigit(c)) return number(scanner);

		switch (c) {
		case '\n':
			scanner->line++;
			resetLength(scanner);
			break;
		case ' ':
		case '\t':
		case '\r':
			resetLength(scanner);
			break;
		case '(': return makeToken(scanner, TOKEN_LEFT_PAREN);
		case ')': return makeToken(scanner, TOKEN_RIGHT_PAREN);
		case '[': return makeToken(scanner, TOKEN_LEFT_BRACKET);
		case ']': return makeToken(scanner, TOKEN_RIGHT_BRACKET);
		case ',': return makeToken(scanner, TOKEN_COMMA);
		case '.': return makeToken(scanner, TOKEN_DOT);
		case ';': return makeToken(scanner, TOKEN_SEMICOLON);
		case '!': return makeToken(scanner, match(scanner, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
		case '=': return makeToken(scanner, match(scanner, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
		case '<': return makeToken(scanner, match(scanner, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
		case '>': return makeToken(scanner, match(scanner, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
		case '-': return makeToken(scanner, match(scanner, '=') ? TOKEN_MINUS_EQUAL : TOKEN_MINUS);
		case '+': return makeToken(scanner, match(scanner, '=') ? TOKEN_PLUS_EQUAL : TOKEN_PLUS);
		case '/':
			switch (peek(scanner)) {
			case '=':
				advance(scanner);
				return makeToken(scanner, TOKEN_SLASH_EQUAL);
			case '/':
				advance(scanner);
				while (peek(scanner) != '\n' && !isAtEnd(scanner)) advance(scanner);
				resetLength(scanner);
				break;
			case '*':
				advance(scanner);
				skipBlockComment(scanner);
				break;
			default:
				return makeToken(scanner, TOKEN_SLASH);
			}
			break;
		case '*':
			switch (peek(scanner))
			{
			case '=':
				advance(scanner);
				return makeToken(scanner, TOKEN_STAR_EQUAL);
			case '/':
				advance(scanner);
				resetLength(scanner);
				break;
			case 'c':
			case 'f':
			case 'i':

				// TODO - end

				break;
			default:
				return makeToken(scanner, TOKEN_STAR);
			}
			break;
		case ':':
			switch (peek(scanner))
			{
			case '=':
				advance(scanner);
				return makeToken(scanner, TOKEN_COLON_COLON);
			case ':':
				advance(scanner);
				return makeToken(scanner, TOKEN_COLON_COLON);
			default:
				return errorToken(scanner, "Unexpected character.");
			}
			break;
		case '\'':
		case '"':
			return string(scanner, c);
		default:
			return errorToken(scanner, "Unexpected character.");
		}
	}

	return makeToken(scanner, TOKEN_EOF);
}
