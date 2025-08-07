# ron

I'm trying to create a language that allows me to define small computational systems as simply as possible.

Example: Peano number addition

```
# Declare operators
op 50 : S _
op 50 : _ plus _ is _
    
# Rule 1
Z plus n is n

# Rule 2
(S n1) plus n2 is (S n) {
    n1 plus n2 is n
}

# Query
main {
    (S Z) plus (S Z) is x
}
```

Example: SKI combinator calculus

```
# Declare operators
op 50 : _ -> _
op 50 : _ => _
op 100 : _ _

# Rule 1
I x     -> x
K x y   -> x
S x y z -> x z (y z)

# Rule 2
x y -> x’ y { x -> x’ }
x y -> x y’ { y -> y’ }

# Rule 3
x => y { x -> y }
x => y { x -> z; z => y }

# Query
main {
    S K S K => x
}
```

Example: Fibonacci function with OCaml-style syntax

```
op 90 : _ _
op 80 : integer _
op 60 : _ + _
op 60 : _ - _
op 60 : _ < _
op 60 : _ >= _
op 50 : _ -> _
op 40 : _ is _
op 40 : _ = _
op 40 : _ \= _
op 30 : _ , _
op 25 : [ _ |- _ ]
op 20 : if _ then _ else _
op 20 : let rec _ _ = _ in _
op 10 : _ |- _ => _   

# c |- e => v means that in environment c, expression e evaluates to value v

# Integers evaluate to themselves
c |- n => n {
    <integer n>
}

# Addition and subtraction
c |- e1 + e2 => v {
    c |- e1 => v1
    c |- e2 => v2
    <v is v1 + v2>
}
c |- e1 - e2 => v {
    c |- e1 => v1
    c |- e2 => v2
    <v is v1 - v2>
}

# Comparison
c |- e1 < e2 => true {
    c |- e1 => v1
    c |- e2 => v2
    <v1 < v2>
}
c |- e1 < e2 => false {
    c |- e1 => v1
    c |- e2 => v2
    <v1 >= v2>
}

# if expression
c |- if e1 then e2 else e3 => v {
    c |- e1 => true
    c |- e2 => v
}
c |- if e1 then e2 else e3 => v {
    c |- e1 => false
    c |- e3 => v
}

# Environment and variables
x = v |- x => v
c, x = v |- x => v
c, y = v' |- x => v {
    <x \= y>
    c |- x => v
}

# let rec function definition
c |- let rec f x = e1 in e2 => v {
    c, f = [c |- f = x -> e1] |- e2 => v
}

# Function application
c |- e1 e2 => v { 
    c |- e1 => [c2 |- f = x -> e0]
    c |- e2 => v2
    c2, f = [c2 |- f = x -> e0] , x = v2 |- e0 => v
}

main {
    0 |- let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 9 => v
}
```

## Goals

- I want to create a language that makes it as easy as possible to implement computational systems like those in "Concepts of Programming Languages" or TaPL.
- Most of what I want to do can be done in Prolog, so I want to make the appearance a little more to my liking.
- Specifically:
    - I want to be able to define mixfix operators to write in a natural grammar.
    - I want to use {} to create an ALGOL-like look.
    - I want to use lowercase letters for variable names.
    - I want to be able to write things like x' by using ' at the end of a variable name.
    - I want to make it so that when main is defined, it becomes the starting point.

## What it does

- It parses in Prolog, converts it 1:1 to a Prolog predicate, and executes it in the Prolog processor.

## What works

- The examples above work.

## Usage

```
$ swipl ron.pl example/if.ron
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.