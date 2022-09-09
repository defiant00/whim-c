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

// property access can be by identifier or string, with this setup:
propName :: 'property'
i :: instance()
i.property := 'yay'
// these are then equivalent:
i.property
i.'property'
i['property']
i[propName]

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

// lists are resizable indexable collections of any type denoted with ()
// () is an empty list, (1,) is a list with one item (the comma is required to distinguish it from grouping)
// (1, 2, 3) items are separated by commas, the final comma is optional
// indexes start at 0
list := (1, 2.3, 'hi', myObj, MyClass, myFunc)
list[1]        // this is 2.3
list[2] = 42   // this has now changed from a string to an int
// negative indexes are equivalent to list.length + index
list[-1] = 'last item'    // this updates the last item in the list

// sets are unordered and unindexed collections of any type denoted with []
// sets cannot contain duplicates
// Set() is the empty set
set := [1, 3, "hi"]
set.add("yay")
set.contains(3)
set[key]  // equivalent to set.contains[key]
set.remove('hi')

// maps are unordered indexable collections of key/value pairs denoted with []
// [] is an empty map
// items are represented as key (:: or :=) value, and separated by commas, the last of which is optional
map := [
  1 :: 'ONE',

  'constantVal' :: 'hi',

  'var' := 3,

  'someFn' :: fn
    return 'hello'
  /fn,
]
map[1]        // 'ONE'
map.var       // 3
map.someFn()  // 'hello'

// Ranges
[from]..[to(exclusive)] [: step]
// from defaults to 0
// to defaults to item.length if applicable
// step defaults to 1
1..3      // 1, 2
..4       // 0, 1, 2, 3
3..       // 3 to length - 1
..        // 0 to length - 1
1..4: 2   // 1, 3
4..1: -1  // 4, 3, 2

// looping
loop
  // infinite loop
  break x < 3       // break and continue take an expression
  continue y > z    // and break or continue if it is truthy
/loop

for i in item   // creates an iterator from item, and i contains the next value
/for

// a semicolon is an explicit empty statement for grammar ambiguities
// this is myList[1]
myList
[1]
// this is two statements, myList and a set literal [1]
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
// list       List          ()
// set        Set           [val1, val2]
// map        Map           [key :: const, key := var, 'string key' :: val]
// range      Range         from..to : step

// Operators: + - * / % ! == != and or :: := = += -= *= /= %= is from

// no implicit string conversion
val := 3 + someStr.num()
message :: "Value is " + val.str()
```

text: *.whim  
compiled: *.whir

## TODO

* better line number encoding
* \> 256 constants
* error handling
* compiler arrays should not be a fixed size
* directly load common number opcodes
* strings - flexible array members
* support other key types for hash tables
* string hash set (instead of table with nil values) for interning
* utf8
* constant deduplication
* separate sized jumps
* have exit jumps jump straight to the end instead of chaining
* don't emit (nil and return) if the last line is already a return
* ip in a local and flag it as a register
* error reporting from native code
* closures only for functions that need it?
* loop var new per item/closure
* more compact object header
* generational gc
* properties by string
