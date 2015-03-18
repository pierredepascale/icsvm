# Scheme Virtual Machine

The purpose of this project is to experiment with a VM using modern
implementation techniques (think V8 or Dart( to run Scheme code.
This is a toy implementation in Scheme itself in order to be easily
modifiable.

# Running the code

To run the code, just calls `(test)`. It sould compute the 10th and 20th 
fibonacci number. A message should also appear indicating that the 
interpreter wants to optimize a the `fib` function. 

After that you can experiment with the `ev` and `compile` procedures.

# Status

Right now Scheme expressions are compiled to an IR that is later 
interpreted. With each function call there is an associated inline cache 
that records at runtime which closure is called at this point.

Currently it is missing an optimizing compiler that produces
optimized code with the knowledge contained in the inline caches.
