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

// scoped blocks
do
  // statements
/do

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
MyClass :: class is Parent      // optional inheritance
  // items here are scoped to the class

  // constructor
  new :: fn(self, x, y)
    self.base.new(self, x, y)   // self.base(params) would create a new object, so instead
                                // explicitly call new with the current instance
    self.x := x
    self.y := y
  /fn

  // method
  myMethod :: fn(self)      // self is a convention, not a requirement
    return self.x + self.y
  /fn

  // static variable
  var := 3

  // functions can be mutable as well
  myFunc := fn(z)
    return z * z
  /fn

  // same with classes, which can also be nested
  NestedClass := class
  /class

  // can also use strings as keys
  'something' :: 42

  // operator overloading is done with strings
  "+" :: fn(a, b)
    return MyClass(a.x + b.x, a.y + b.y)
  /fn
/class

// initialization is just the type used as a function,
// this automatically creates a new instance and passes it in as the first argument
myInst :: MyClass(1, 2)

// a class is a map with a few specific values defined
// name - the name of the type
// base - optional base class
// new  - optional constructor

// lists are resizable indexable collections of any type
// indexes start at 0
list := [1, 2.3, 'hi', myObj, MyClass, myFunc]
list[1]        // this is 2.3
list[2] = 42   // this has now changed from a string to an int
// negative indexes are equivalent to list.length + index
list[-1] = 'last item'    // this updates the last item in the list

// Ranges
[from..to(exclusive)]
// from always defaults to 0
// to defaults to item.length if applicable
list[..] is the whole list

// a range is stored as start and end and can be iterated
[1..4].list() == [1, 2, 3, 4]    // convert to a list
// can specify step
[1..4, 2]
[4..1, -1]


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
// map        Map           [key :: const, key := var, 'string key' :: val]
// range      Range         [from..to] [from..to, step]

// Operators: + - * / % ! == != and or :: := = += -= *= /= %= is

// no implicit string conversion
val := 3 + someStr.num()
message :: "Value is " + val.str()
```

text: *.whim  
compiled: *.whir

## TODO

* use `from` or `is` for inheritance?
* better line number encoding
* \> 256 constants
* error handling
* directly load common number opcodes
* strings - flexible array members
* support other key types for hash tables
* string hash set (instead of table with nil values) for interning?
* utf8
* constant deduplication
* op pop n
* separate sized jumps
* have exit jumps jump straight to the end instead of chaining
* don't emit (nil and return) if the last line is already a return
* ip in a local and flag it as a register
* error reporting from native code
* closures only for functions that need it?
* loop var new per item/closure
