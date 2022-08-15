# Whimsy
Simple embeddable programming language.

---

```
// Whimsy, a simple embeddable language.

/* muliline
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

// function
myFunc :: fn(x, y, z)
/fn

// all functions are static and require an explicit self

// blocks end with /block, eg /fn, /if, /for

// class
MyClass :: class is Parent    // optional inheritance
  // items here are scoped to the class

  // constructor
  new :: fn(self, x, y)
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

// initialization is just the class used as a function
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

// this is a - b without a semicolon
a
-b

// Type       Type Name
// ---------  ---------
// int/f64    Number        (automatic big int as necessary)
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
