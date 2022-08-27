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

static void advanceMulti(Scanner* scanner, int count) {
	scanner->current += count;
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

static bool isAlphaOrDigit(char c) {
	return isAlpha(c) || isDigit(c);
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
		case '\n':
			scanner->line++;
			break;
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
	case '_':
		if (scanner->current - scanner->start == 1) {
			return TOKEN_UNDERSCORE;
		}
		break;
	case 'a': return checkKeyword(scanner, 1, 2, "nd", TOKEN_AND);
	case 'b': return checkKeyword(scanner, 1, 4, "reak", TOKEN_BREAK);
	case 'c':
		if (scanner->current - scanner->start > 1) {
			switch (scanner->start[1]) {
			case 'a': return checkKeyword(scanner, 2, 3, "tch", TOKEN_CATCH);
			case 'l': return checkKeyword(scanner, 2, 3, "ass", TOKEN_CLASS);
			case 'o': return checkKeyword(scanner, 2, 6, "ntinue", TOKEN_CONTINUE);
			}
		}
		break;
	case 'd': return checkKeyword(scanner, 1, 1, "o", TOKEN_DO);
	case 'e':
		if (scanner->current - scanner->start == 4 && scanner->start[1] == 'l') {
			switch (scanner->start[2]) {
			case 'i': return checkKeyword(scanner, 3, 1, "f", TOKEN_ELIF);
			case 's': return checkKeyword(scanner, 3, 1, "e", TOKEN_ELSE);
			}
		}
		break;
	case 'f':
		if (scanner->current - scanner->start > 1) {
			switch (scanner->start[1])
			{
			case 'a': return checkKeyword(scanner, 2, 3, "lse", TOKEN_FALSE);
			case 'i': return checkKeyword(scanner, 2, 5, "nally", TOKEN_FINALLY);
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
			case 'n': return TOKEN_IN;
			case 's': return TOKEN_IS;
			}
		}
		break;
	case 'n': return checkKeyword(scanner, 1, 2, "il", TOKEN_NIL);
	case 'o': return checkKeyword(scanner, 1, 1, "r", TOKEN_OR);
	case 'r': return checkKeyword(scanner, 1, 5, "eturn", TOKEN_RETURN);
	case 't':
		if (scanner->current - scanner->start > 1) {
			switch (scanner->start[1]) {
			case 'h': return checkKeyword(scanner, 2, 3, "row", TOKEN_THROW);
			case 'r':
				if (scanner->current - scanner->start > 2) {
					switch (scanner->start[2]) {
					case 'u': return checkKeyword(scanner, 3, 1, "e", TOKEN_TRUE);
					case 'y': if (scanner->current - scanner->start == 3) return TOKEN_TRY;
					}
				}
			}
		}
		break;
	}
	return TOKEN_IDENTIFIER;
}

static Token identifier(Scanner* scanner) {
	while (isAlphaOrDigit(peek(scanner))) advance(scanner);
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
		case ':':
			switch (peek(scanner))
			{
			case ':':
				advance(scanner);
				return makeToken(scanner, TOKEN_COLON_COLON);
			case '=':
				advance(scanner);
				return makeToken(scanner, TOKEN_COLON_EQUAL);
			}
			return errorToken(scanner, "Unexpected character.");
		case '!': return makeToken(scanner, match(scanner, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
		case '=': return makeToken(scanner, match(scanner, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
		case '<': return makeToken(scanner, match(scanner, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
		case '>': return makeToken(scanner, match(scanner, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
		case '+': return makeToken(scanner, match(scanner, '=') ? TOKEN_PLUS_EQUAL : TOKEN_PLUS);
		case '-': return makeToken(scanner, match(scanner, '=') ? TOKEN_MINUS_EQUAL : TOKEN_MINUS);
		case '%': return makeToken(scanner, match(scanner, '=') ? TOKEN_PERCENT_EQUAL : TOKEN_PERCENT);
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
			default:
				return makeToken(scanner, TOKEN_STAR);
			}
			break;
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
			case 'c':
				if (scanner->current[1] == 'l' &&
					scanner->current[2] == 'a' &&
					scanner->current[3] == 's' &&
					scanner->current[4] == 's' &&
					!isAlphaOrDigit(scanner->current[5])) {
					advanceMulti(scanner, 5);
					return makeToken(scanner, TOKEN_CLASS_END);
				}
				return makeToken(scanner, TOKEN_SLASH);
			case 'd':
				if (scanner->current[1] == 'o' && !isAlphaOrDigit(scanner->current[2])) {
					advanceMulti(scanner, 2);
					return makeToken(scanner, TOKEN_DO_END);
				}
				return makeToken(scanner, TOKEN_SLASH);
			case 'f':
				switch (scanner->current[1]) {
				case 'n':
					if (!isAlphaOrDigit(scanner->current[2])) {
						advanceMulti(scanner, 2);
						return makeToken(scanner, TOKEN_FN_END);
					}
					break;
				case 'o':
					if (scanner->current[2] == 'r' && !isAlphaOrDigit(scanner->current[3])) {
						advanceMulti(scanner, 3);
						return makeToken(scanner, TOKEN_FOR_END);
					}
					break;
				}
				return makeToken(scanner, TOKEN_SLASH);
			case 'i':
				if (scanner->current[1] == 'f' && !isAlphaOrDigit(scanner->current[2])) {
					advanceMulti(scanner, 2);
					return makeToken(scanner, TOKEN_IF_END);
				}
				return makeToken(scanner, TOKEN_SLASH);
			case 't':
				if (scanner->current[1] == 'r' &&
					scanner->current[2] == 'y' &&
					!isAlphaOrDigit(scanner->current[3])) {
					advanceMulti(scanner, 3);
					return makeToken(scanner, TOKEN_TRY_END);
				}
				return makeToken(scanner, TOKEN_SLASH);
			default:
				return makeToken(scanner, TOKEN_SLASH);
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
