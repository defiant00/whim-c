#ifndef whimsy_compiler_h
#define whimsy_compiler_h

#include "object.h"
#include "vm.h"

bool compile(VM* vm, const char* source, Chunk* chunk);

#endif
