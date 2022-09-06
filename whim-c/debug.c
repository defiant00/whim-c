#include <stdio.h>

#include "debug.h"
#include "object.h"
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

static int byteInstruction(const char* name, Chunk* chunk, int offset) {
	uint8_t index = chunk->code[offset + 1];
	printf("%-20s %4d\n", name, index);
	return offset + 2;
}

static int jumpInstruction(const char* name, int sign, Chunk* chunk, int offset) {
	uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
	jump |= chunk->code[offset + 2];
	printf("%-20s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
	return offset + 3;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
	uint8_t constant = chunk->code[offset + 1];
	printf("%-20s %4d '", name, constant);
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
	case OP_DEFINE_GLOBAL_CONST:	return constantInstruction("def global const", chunk, offset);
	case OP_DEFINE_GLOBAL_VAR:		return constantInstruction("def global var", chunk, offset);
	case OP_GET_GLOBAL:				return constantInstruction("get global", chunk, offset);
	case OP_SET_GLOBAL:				return constantInstruction("set global", chunk, offset);
	case OP_ADD_SET_GLOBAL:			return constantInstruction("add set global", chunk, offset);
	case OP_SUBTRACT_SET_GLOBAL:	return constantInstruction("sub set global", chunk, offset);
	case OP_MULTIPLY_SET_GLOBAL:	return constantInstruction("mul set global", chunk, offset);
	case OP_DIVIDE_SET_GLOBAL:		return constantInstruction("div set global", chunk, offset);
	case OP_MODULUS_SET_GLOBAL:		return constantInstruction("mod set global", chunk, offset);
	case OP_GET_LOCAL:				return byteInstruction("get local", chunk, offset);
	case OP_SET_LOCAL:				return byteInstruction("set local", chunk, offset);
	case OP_ADD_SET_LOCAL:			return byteInstruction("add set local", chunk, offset);
	case OP_SUBTRACT_SET_LOCAL:		return byteInstruction("sub set local", chunk, offset);
	case OP_MULTIPLY_SET_LOCAL:		return byteInstruction("mul set local", chunk, offset);
	case OP_DIVIDE_SET_LOCAL:		return byteInstruction("div set local", chunk, offset);
	case OP_MODULUS_SET_LOCAL:		return byteInstruction("mod set local", chunk, offset);
	case OP_GET_UPVALUE:			return byteInstruction("get upvalue", chunk, offset);
	case OP_SET_UPVALUE:			return byteInstruction("set upvalue", chunk, offset);
	case OP_ADD_SET_UPVALUE:		return byteInstruction("add set upvalue", chunk, offset);
	case OP_SUBTRACT_SET_UPVALUE:	return byteInstruction("sub set upvalue", chunk, offset);
	case OP_MULTIPLY_SET_UPVALUE:	return byteInstruction("mul set upvalue", chunk, offset);
	case OP_DIVIDE_SET_UPVALUE:		return byteInstruction("div set upvalue", chunk, offset);
	case OP_MODULUS_SET_UPVALUE:	return byteInstruction("mod set upvalue", chunk, offset);
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
	case OP_JUMP:					return jumpInstruction("jump", 1, chunk, offset);
	case OP_JUMP_BACK:				return jumpInstruction("jump back", -1, chunk, offset);
	case OP_JUMP_IF_TRUE:			return jumpInstruction("jump if true", 1, chunk, offset);
	case OP_JUMP_IF_FALSE:			return jumpInstruction("jump if false", 1, chunk, offset);
	case OP_JUMP_IF_TRUE_POP:		return jumpInstruction("jump if true (pop)", 1, chunk, offset);
	case OP_JUMP_IF_FALSE_POP:		return jumpInstruction("jump if false (pop)", 1, chunk, offset);
	case OP_CALL:					return byteInstruction("call", chunk, offset);
	case OP_CLOSURE: {
		offset++;
		uint8_t constant = chunk->code[offset++];
		printf("%-20s %4d ", "closure", constant);
		printValue(chunk->constants.values[constant]);
		printf("\n");

		ObjFunction* function = AS_FUNCTION(chunk->constants.values[constant]);
		for (int j = 0; j < function->upvalueCount; j++) {
			int isLocal = chunk->code[offset++];
			int index = chunk->code[offset++];
			printf("%04d      > %s %d\n", offset - 2,
				isLocal ? "local" : "upvalue", index);
		}

		return offset;
	}
	case OP_CLOSE_UPVALUE:			return simpleInstruction("close upvalue", offset);
	case OP_RETURN:					return simpleInstruction("return", offset);
	case OP_CLASS:					return constantInstruction("class", chunk, offset);
	case OP_ANON_CLASS:				return simpleInstruction("anon class", offset);
	default:
		printf("Unknown opcode %d\n", instruction);
		return offset + 1;
	}
}
