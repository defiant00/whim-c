#ifndef whimsy_chunk_h
#define whimsy_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
	OP_CONSTANT,
	OP_NIL,
	OP_TRUE,
	OP_FALSE,
	OP_POP,
	OP_DEFINE_GLOBAL_CONST,
	OP_DEFINE_GLOBAL_VAR,
	OP_GET_GLOBAL,
	OP_SET_GLOBAL,
	OP_ADD_SET_GLOBAL,
	OP_SUBTRACT_SET_GLOBAL,
	OP_MULTIPLY_SET_GLOBAL,
	OP_DIVIDE_SET_GLOBAL,
	OP_MODULUS_SET_GLOBAL,
	OP_EQUAL,
	OP_NOT_EQUAL,
	OP_GREATER,
	OP_GREATER_EQUAL,
	OP_LESS,
	OP_LESS_EQUAL,
	OP_ADD,
	OP_SUBTRACT,
	OP_MULTIPLY,
	OP_DIVIDE,
	OP_MODULUS,
	OP_NEGATE,
	OP_NOT,
	OP_RETURN,
} OpCode;

typedef struct {
	int count;
	int capacity;
	uint8_t* code;
	int* lines;
	ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif
