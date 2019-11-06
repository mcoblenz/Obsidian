Assets
======
Some owning references are to things that should not be accidentally lost. To help prevent accidental loss, we can mark contracts as *assets*. Let's do this for ``Money``:

::

   contract Money { // Money is an asset
   }

Now, it is the programmer's responsibility to pay special attention to owned references to assets. For example:

::

   function test() public {
      Money m = ...; // Assume m is owned here
      // BUG: Money is an asset, so owned references should not be allowed to go out of scope!
   }


We can fix this by (for example) returning m, assigning it to an owning field, or passing it as an argument to an appropriate function. For example:

::

   function test() public returns (Money) { // returns an owned reference
      Money m = ...; // assume m is owned
      return m; // gives ownership of m to the caller of test()
   }

NOTE: non-owning references to ``Money`` are not restricted; there's no problem with letting them go out of scope.

