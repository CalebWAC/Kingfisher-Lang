# The Kingfisher Programming Language
Kingfisher is a programming language designed with built-in syntax for an entity component system with modern features, inspired by F#, Swift, and Haxe. Some features of the language include:
- Everything is an expression
- Support for custom types (records, discriminated unions, tuples)
- Flexible whitespace-based syntax (to be implemented)
- ECS without any extra libraries (to be implemented)


 The specification file details the syntax of the language, and the grammar file has the specific grammar used to parse the language.

## Current State
Currently, only a subset of the language is functional. This includes bindings, basic control flow, union and record types, and a standard library of one function, println. As the parser is improved, the capabilities of the language will expand to its written standard. Kingfisher is compiled to Haxe, so in order to run a Kingfisher program, one will need the Haxe compiler installed.

## Sample code
```
let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

for e in arr do
    if e % 2 = 0 then println e 
    else println 'Odd number'


fun add a b = a + b
add 3 4

when x is
    1, 2 -> ()
    1..5 -> ()
    (_, 'a') -> ()
    { _, field } -> ()
    [_, 1, 2] -> ()
    Color.Red -> ()
    Some num where num = 5 -> ()
    _ -> ()

// ECS 

// Entities
ent player = Transform PlayerController
ent 10 = Transform Collider

// Components
com Transform =  
    position: Vec3
    rotation: Vec3
    scale: Vec3
Transform@player

// System
sys Transform Enemy | Update =
    if keyPressed Key.Left then Transform.position.x <-+ 1
```