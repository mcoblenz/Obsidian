Assets
======
Some owning references are to things that should not be accidentally lost. To prevent accidental loss, we can declare contracts 
with the keyword ``asset``. Let's do this for ``Money``:

::

   asset contract Money {
   }

Now, if we accidentally lose track of an owning reference to a ``Money`` object (by letting it go out of scope without transferring ownership to somewhere else, such as a field), the compiler will give an error:

::

   transaction test() {
      Money m = ...; [m@Owned]; // OK, m is Owned here
      // ERROR: cannot drop reference to owned asset m
   }


We can fix this by (for example) returning m, assigning it to an owning field, or passing it as an argument to an appropriate transaction. For example:

::

   transaction test() returns Money@Owned {
      Money m = ...; [m@Owned]; // OK, m is Owned here
      return m; // gives ownership of m to the caller of test()
   }

NOTE: non-owning references to ``Money`` are not restricted; the compiler gives no errors when they go out of scope.

