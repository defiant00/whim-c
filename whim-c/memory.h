#ifndef whimsy_memory_h
#define whimsy_memory_h

#include "common.h"
#include "object.h"
#include "vm.h"

#define ALLOCATE(type, count) \
	(type*)reallocate(vm, NULL, 0, sizeof(type) * (count))

#define FREE(type, pointer) reallocate(vm, pointer, sizeof(type), 0)

#define GROW_CAPACITY(capacity) \
	((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
	(type*)reallocate(vm, pointer, sizeof(type) * (oldCount), \
		sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \
	reallocate(vm, pointer, sizeof(type) * (oldCount), 0)

void* reallocate(VM* vm, void* pointer, size_t oldSize, size_t newSize);
void markObject(Obj* object);
void markValue(Value value);
void collectGarbage(VM* vm);
void freeObjects(VM* vm);

#endif
