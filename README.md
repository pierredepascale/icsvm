# Scheme Virtual Machine

The purpose of this project is to experiment with a VM using modern
implementation techniques (think V8 or Dart( to run Scheme code.
This is a toy implementation in Scheme itself in order to be easily
modifiable.

# Running the code

To run the code, just calls `(test)`. After that you can experiment 
with the `ev` and `compile`

# Status

Right now Scheme expressions are compiled to an IR that later 
interpreted. With each function call there is an associated inline cache 
that record at runtime which closure is called.

Missing are triggers that causes a closure to be recompiled and 
optimized with the knowledge contained in the inline caches.
