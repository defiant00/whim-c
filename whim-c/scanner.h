#ifndef whimsy_scanner_h
#define whimsy_scanner_h

typedef struct {
	const char* start;
	const char* current;
	int line;
} Scanner;

typedef enum {
	// Single-character tokens
	TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
	TOKEN_LEFT_BRACKET, TOKEN_RIGHT_BRACKET,
	TOKEN_COMMA,
	TOKEN_DOT,
	TOKEN_SEMICOLON,
	TOKEN_UNDERSCORE,
	// One or two character tokens
	TOKEN_BANG, TOKEN_BANG_EQUAL,
	TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
	TOKEN_LESS, TOKEN_LESS_EQUAL,
	TOKEN_GREATER, TOKEN_GREATER_EQUAL,
	TOKEN_MINUS, TOKEN_MINUS_EQUAL,
	TOKEN_PLUS, TOKEN_PLUS_EQUAL,
	TOKEN_SLASH, TOKEN_SLASH_EQUAL,
	TOKEN_STAR, TOKEN_STAR_EQUAL,
	TOKEN_PERCENT, TOKEN_PERCENT_EQUAL,
	// Two character tokens
	TOKEN_COLON_COLON, TOKEN_COLON_EQUAL,
	// Literals
	TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,
	// Keywords
	TOKEN_AND,
	TOKEN_BREAK,
	TOKEN_CLASS, TOKEN_CONTINUE,
	TOKEN_DO,
	TOKEN_ELSE,
	TOKEN_FALSE, TOKEN_FN, TOKEN_FOR, TOKEN_FROM,
	TOKEN_IF, TOKEN_IN, TOKEN_IS,
	TOKEN_NIL,
	TOKEN_OR,
	TOKEN_RETURN,
	TOKEN_TRUE,

	// Ends
	TOKEN_CLASS_END,
	TOKEN_DO_END,
	TOKEN_FN_END,
	TOKEN_FOR_END,
	TOKEN_IF_END,

	TOKEN_ERROR, TOKEN_EOF,
} TokenType;

typedef struct {
	TokenType type;
	const char* start;
	int length;
	int line;
} Token;

void initScanner(Scanner* scanner, const char* source);
Token scanToken(Scanner* scanner);

#endif
