# lesson 11 - controlling encapsulation

In Haskell, when we want to define an *abstract* datatype which hides its
internals, the usual idiom is to give the type its own module and carefully
exclude the constructor and any field accessor from the export list.

That way, other modules have to use the approved public interface to manipulate
values of the datapype, and can't carelessly fiddle with values' internals.

On the other hand, it can be useful to have some way of inspecting the internal
structure of a value, if only to perform print-style debugging.

But this, of course, breaks encapsulation! How to reconcile both requirements?

We begin with the observation is that we don't require complete transparency
all the time. Regular *program logic* in a module should never have access to
the internals of datatypes in other modules. But *framework code* might need to
break encapsulation for the purposes of logging, and we might break it during a
REPL session for the purposes of debugging.

How to ensure that abstract datatypes are opaque to program logic but
transparent to "framework" code?

This lesson contains a possible solution using Backpack.

The structure of the code is as follows:

- The main library—containing "program logic"—defines an abstract datatype
  `Foo` in module [`Lesson11.Foo`](./lib/Lesson11/Foo.hs), which is imported by
  module [`Lesson11.UsesFoo`](./lib/Lesson11/UsesFoo.hs).

- The library also defines a typeclass called [`Inspectable`](./lib/Lesson11/Inspectable.hs). `Foo` has an
  `Inspectable` instance, which treatens to
  break encapsulation. We don't want the code from `UsesFoo` to make any
  decision based on the result of inspecting `Foo` values!

- Here's the trick: the main library is *indefinite*: the result of the `inspect`
  method from `Inspection` come wrapped in an abstract (in the Backpack sense)
  type constructor called [`Mystery`](./lib/Lesson11/Mystery.hsig). 

- Even if `UsesFoo` calls `inspect` on `Foo` values, it can't do anything
  useful with the result, because the module signature provides no functions
  for working with `Mystery String`s.

- However, the [test suite](./lib/test/tests.hs)—which we consider "framework
  code"—provides an implementation module which matches `Mystery` with the
  `Identity` functor.  Meaning that it *can* actually use the results of
  `inspect`!

Run the tests with:

> cabal test lesson11-controlling-encapsulation:test:tests

## Some doubts 

- Can something like this be done in Haskell without module signatures?

- How does this compare with making the datatypes themselves *wholly abstract*
  using module signatures? 

  In terms of this example, making `Foo` abstract in the Backpack sense (not in
  the "hide the constructor") sense,  move the implementation to a separate
  package, and make the program logic work only with the signature.

## See also

- [This tweet](https://twitter.com/jonmsterling/status/1400459227223576580)
  that goes over my head but seems to point to a principled type-theoretical
  mechanism for controlling encapsulation.

  Section 2.2.3 "Reconciling debugging with abstraction" of the paper ["A
  metalanguage for multi-phase
  modularity"](http://www.jonmsterling.com/pdfs/phml.pdf) by Sterling and
  Harper.
