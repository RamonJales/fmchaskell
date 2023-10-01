# fmchaskell

This repository contains exercises done in the Haskell functional programming language.

We start by defining, in the "nat.hs" file, the Nat data type. We then define some functions used for this type of data, such as: addition, multiplication, exponentiation, among others. All of them recursively. The possible forms of a Nat are the O or a success of some Nat(S). The beauty of this is that we have just built the natural numbers to infinity.

When defining some funtions for the Nats, we came across some situations that would be interesting return a Bool value insted of Nat value. The language C, reuse the type Int to implement the type Bool. But this is not a good idea, in some cases they can be confused. Whenever we need a new data type to represent and manipulate in our functions, we wil create them. We can create: "weekday", "Bool", "Empty", "Void"... datatype. So we will create Bool data type to use in some functions. For example, a funtion to know if a number "n" is divided for a number "m".