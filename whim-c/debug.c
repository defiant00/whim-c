#include <stdio.h>

#include "debug.h"
#include "value.h"

void disassembleChunk(Chunk* chunk, const char* name) {
	printf("== %s ==\n", name);

	for (int offset = 0; offset < chunk->count;) {
		offset = disassembleInstruction(chunk, offset);
	}
}

static int simpleInstruction(const char* name, int offset) {
	printf("%s\n", name);
	return offset + 1;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
	uint8_t constant = chunk->code[offset + 1];
	printf("%-16s %4d '", name, constant);
	printValue(chunk->constants.values[constant]);
	printf("'\n");
	return offset + 2;
}

int disassembleInstruction(Chunk* chunk, int offset) {
	printf("%04d ", offset);

	if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
		printf("   | ");
	}
	else {
		printf("%4d ", chunk->lines[offset]);
	}

	uint8_t instruction = chunk->code[offset];
	switch (instruction)
	{
	case OP_CONSTANT:				return constantInstruction("constant", chunk, offset);
	case OP_NIL:					return simpleInstruction("nil", offset);
	case OP_TRUE:					return simpleInstruction("true", offset);
	case OP_FALSE:					return simpleInstruction("false", offset);
	case OP_POP:					return simpleInstruction("pop", offset);
	case OP_DEFINE_GLOBAL_CONST:	return constantInstruction("def g.const", chunk, offset);
	case OP_DEFINE_GLOBAL_VAR:		return constantInstruction("def g.var", chunk, offset);
	case OP_GET_GLOBAL:				return constantInstruction("get global", chunk, offset);
	case OP_SET_GLOBAL:				return constantInstruction("set global", chunk, offset);
	case OP_ADD_SET_GLOBAL:			return constantInstruction("add set global", chunk, offset);
	case OP_SUBTRACT_SET_GLOBAL:	return constantInstruction("sub set global", chunk, offset);
	case OP_MULTIPLY_SET_GLOBAL:	return constantInstruction("mul set global", chunk, offset);
	case OP_DIVIDE_SET_GLOBAL:		return constantInstruction("div set global", chunk, offset);
	case OP_MODULUS_SET_GLOBAL:		return constantInstruction("mod set global", chunk, offset);
	case OP_EQUAL:					return simpleInstruction("equal", offset);
	case OP_NOT_EQUAL:				return simpleInstruction("not equal", offset);
	case OP_GREATER:				return simpleInstruction("greater", offset);
	case OP_GREATER_EQUAL:			return simpleInstruction("greater equal", offset);
	case OP_LESS:					return simpleInstruction("less", offset);
	case OP_LESS_EQUAL:				return simpleInstruction("less equal", offset);
	case OP_ADD:					return simpleInstruction("add", offset);
	case OP_SUBTRACT:				return simpleInstruction("subtract", offset);
	case OP_MULTIPLY:				return simpleInstruction("multiply", offset);
	case OP_DIVIDE:					return simpleInstruction("divide", offset);
	case OP_MODULUS:				return simpleInstruction("modulus", offset);
	case OP_NEGATE:					return simpleInstruction("negate", offset);
	case OP_NOT:					return simpleInstruction("not", offset);
	case OP_RETURN:					return simpleInstruction("return", offset);
	default:
		printf("Unknown opcode %d\n", instruction);
		return offset + 1;
	}
}
