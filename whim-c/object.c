#include <stdio.h>
#include <string.h>

#include "object.h"
#include "value.h"

static void printFunction(ObjFunction* function) {
	if (function->name != NULL) {
		printf("<fn %s>", function->name->chars);
	}
	else {
		printf("<script>");
	}
}

void printObject(Value value) {
	switch (OBJ_TYPE(value)) {
	case OBJ_FUNCTION:
		printFunction(AS_FUNCTION(value));
		break;
	case OBJ_STRING:
		printf("%s", AS_CSTRING(value));
		break;
	}
}
