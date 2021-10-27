# Crisp

This is a rewrite of Crisp in idris so i can figure out what i'm doing
more easily than the rust version.

## On the difficulty of compiling fexprs

Traditionally, lisps are compiled by recognising special forms and
implementing macro expansion. Crisp does not do this, instead choosing
a path similar to kernel lisp, which has first class 'f-expressions'.

Promoting even the base forms of the language to first class entities
means that the program has no meaning without the environment that it
is compiled in. We do not a priori know what it is doing by looking at
i, so compiling a program involves a certain amount of evaluating it
in an environment. It thus became 'common knowledge' that fexprs could
not be compiled.

Technology has marched along since then and we've learned things about
building languages in the meantime. We've also seen the rise of the
JIT compiler, which turns many notions of dynamism and performance on
their head. We think with careful design, you can compile (most) fexprs just
fine!

In some cases the user will explicitly be relying on dynamic behaviour
and it may be necessary to fall back to the interpreter. We will see
in practice how much of a problem this poses...

Yes, this is an open research problem.

## A mixed programming model

Morally, crisp is a scripting language. It is designed for abstractive
power and flexibility. Our job as compiler authors is to make that
fast *somehow*.

To make the code fast somehow, we attempt to do as much work as
possible at compile time. We figure this will eventually involve a JIT
compiler of some kind.

One day we would like to emit webassembly or such. Maybe just for
programs we can prove don't need an interpreter at runtime. Maybe we
can also ship the interpreter otherwise?

## Difficulties

Optimisation/Recompilation:
 * mutability
 * tracking assumptions that might fail
 * 



At the top level, we start with evaluation because the environment is fully defined.

When 

## Pattern matching

```crisp
(defn fib
  ([0] 0)
  ([n] (+ n (fib (- n 1)))))





```




