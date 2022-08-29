#ifndef whimsy_compiler_h
#define whimsy_compiler_h

#include "object.h"
#include "vm.h"

ObjFunction* compile(VM* vm, const char* source);

#endif
