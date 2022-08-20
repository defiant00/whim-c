# Whimsy
Simple embeddable programming language.

C implementation based on [Crafting Interpreters](http://craftinginterpreters.com/), to be used as a performance baseline when evaluating other options.

---

```
// Whimsy, a simple embeddable language.

/* multiline
   comments

   /* nesting */ is also allowed
*/

// A closing comment token on its own is valid (and ignored)
*/

// constant and variable declarations
myConstant :: "hello"
myVariable := "initialized"

// variables must be declared before use, assignment or other usage without declaration is an error
myVar := 0
myVar = 7

// you can declare and assign multiple items by separating them with commas
first, second = second, first

// lists, maps, and objects can be destructured
first, second := from myList
// is the same as
first := myList[0]
second := myList[1]

first, second := from myObject
// is the same as
first := myObject.first
second := myObject.second

// _ can be used to skip/discard a value
first, _, third := from myList

// mutability is per level, so a constant can have mutable fields

// import modules with use - the module is returned, so assign it to a variable or constant
std :: use Some.Standard.Library

// or maybe with a function?
lib :: std.import(Some.Standard.Library)

// function
myFunc :: fn(x, y, z)
/fn

// all functions are static and require an explicit self

// these are equivalent
first.func(second)
func(first, second)

// name resolution order: object, type, base type (recursively), global

// blocks end with /block, eg /fn, /if, /for

// class
MyClass :: class is Parent    // optional inheritance
  // items here are scoped to the class

  // constructor
  new :: fn(self, x, y)
    self.base(x, y)       // call the base type constructor, passing in self (by using self.)
                          // so it does not create a new object
    self.x := x
    self.y := y
  /fn

  // method
  myMethod :: fn(self)      // self is a convention, not a requirement
    return self.x + self.y
  /fn

  // function
  myFunc :: fn(z)
    return z * z
  /fn
/class

// initialization is just the class used as a function,
// this automatically creates a new instance and passes it in as the first argument
myInst :: MyClass(1, 2)

// lists are resizable indexable collections of any type
// indexes start at 0
list := [1, 2.3, 'hi', myObj, MyClass, myFunc]
list[1]        // this is 2.3
list[2] = 42   // this has now changed from a string to an int

// a semicolon is an explicit empty statement for grammar ambiguities
// this is myList[1]
myList
[1]
// this is two statements, myList and a list literal [1]
myList;
[1]

// without a semicolon this is a - b
a
-b

// Type       Type Name
// ---------  ---------
// f64        Number
// string     String        "", ''
// boolean    Bool          true, false
// nil        Nil           nil
// function   Function      fn
// class      Class         class
// ---------  ---------
// list       List          []
// map        Map           [key = val]

// Operators: + - * / % ! == != and or = += -= *= /= %= is

// no implicit string conversion
val := 3 + someStr.num()
message := "Value is " + val.str()
```

text: *.whim  
compiled: *.whir

## TODO

* mod op
* better line number encoding
* \> 256 constants
* error handling
* directly load common number opcodes
* escaped characters in strings
* strings - flexible array members
* support other key types for hash tables
* string hash set (instead of table with nil values) for interning?
* utf8
* add synchronization points for new keywords like break and continue
* don't allow assignment to constants
* constant deduplication
