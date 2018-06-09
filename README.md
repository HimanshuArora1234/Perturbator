# Perturbator

The goal of this exercise is to explore ways to introduce little errors to values of various types.
We are going to define a perturbation for a type T as a function with the signature T => T.
There are many possible definitions of an error given a value of a particular types; we are just going to arbitrarily define those for a few types:
- Int: a random number from 0% to 10% of the perturbated Int is added to or removed from it.
- String: a random character is removed, or a random printable ASCII character is inserted at a random place
- Vector of any perturable type: a random element is removed, or a random element is duplicated, perturbated and inserted at a random place

Implement this functionality for all possible case classes which fields are of a type which values can be perturbated, potentially recursively. This is done by perturbating each field.

--------------------------

## Implementation

This project uses Scala **type class** and **Shapeless's automatic type class derivation**. 

