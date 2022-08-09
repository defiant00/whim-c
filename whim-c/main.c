#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char* argv[]) {
	Chunk chunk;
	initChunk(&chunk);

	int constant = addConstant(&chunk, 4.2);
	writeChunk(&chunk, OP_CONSTANT, 13);
	writeChunk(&chunk, constant, 13);

	writeChunk(&chunk, OP_RETURN, 13);

	disassembleChunk(&chunk, "test chunk");

	freeChunk(&chunk);

	return 0;
}
