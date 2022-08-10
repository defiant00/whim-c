#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main(int argc, const char* argv[]) {
	VM vm;
	initVM(&vm);

	Chunk chunk;
	initChunk(&chunk);

	int constant = addConstant(&chunk, 4.2);
	writeChunk(&chunk, OP_CONSTANT, 13);
	writeChunk(&chunk, constant, 13);

	constant = addConstant(&chunk, 1.3);
	writeChunk(&chunk, OP_CONSTANT, 13);
	writeChunk(&chunk, constant, 13);

	writeChunk(&chunk, OP_DIVIDE, 13);


	writeChunk(&chunk, OP_RETURN, 15);

	disassembleChunk(&chunk, "test chunk");

	interpret(&vm, &chunk);

	freeVM(&vm);
	freeChunk(&chunk);

	return 0;
}
