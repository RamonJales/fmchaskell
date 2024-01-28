# fmchaskell

## Goals

This repository contains exercises from the subject Mathematical Foundations for Computing(FMC) done in the Haskell functional programming language.

## The Nats

We start by defining, in the "nat.hs" file, the Nat data type. We then define some functions used for this type of data, such as: addition, multiplication, exponentiation, among others. All of them recursively. The possible forms of a Nat are the `O` or a success of some Nat `S`. These things are not the natural things themselves, which mathematicians use in pure mathematics or science. They are just `things` that seem familiar to us with the natural numbers we know, but they don't need to respect the axioms, or rather, they don't even know that the axioms exist. We dont need know how the natural numbers behave.`It's just things that we build with recursion that look like a thing called a natural number`.

## Need for other types

When defining some funtions for the Nats, we came across some situations that would be interesting return a Bool value insted of Nat value. The language C, reuse the type Int to implement the type Bool. But this is not a good idea, in some cases they can be confused. Whenever we need a new data type to represent and manipulate in our functions, we wil create them. We can create: "weekday", "Bool", "Empty", "Void"... datatypes.

That is, always we need a type to our function, we will create this type(if its does not exist). This make it easy to avoid ambiguity and makes our function more readble and makes more sense.

## Lists type
We define in haskell the list type which has two constructors `Nil` or `Empty` and `Cons`. The `Nil` you can thought that is the start of the list and the `cons` like the continuation of the list. The constructor `cons` takes with it an object(any data type).

## Integer implementation
We define the specification(in integer theory subject) and here we implement these specification. So we need define who is the `zero`, the `one`, who is the operations defined in the specifications(these operations need respect the axioms). For example, it does not matter how the `sum` function works, what matter is that it takes two integers and return an integer(according to specification).

## Trees

We define two main types of trees: `BinTree` and `LTree`. The `LTree` is the tree that we are acquainted, the tree that apear when you search for "tree data structure". The `BinTree` is a tree where the knot has no information, its just a knot. 

## Orderdenation functions
Here we define ordenation functions seen in algorithms subject.

## How to compile
First:
```
ghci 
```
Second:
```
:l name-file.hs
```
if you alredy did the "second" step, compile with:
```
:r 
```
Test functin:
```Haskell
search 3 (Fork (Fork (Tip 2) (Tip 0)) (Tip 3))
```
