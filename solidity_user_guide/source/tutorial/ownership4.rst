Ownership -- Miscellaneous
============================


Getting rid of ownership
--------------------------
If ownership is no longer desired, you can write that ownership is being disowned. For example:

::

   contract Money {
       int amount;

       function merge(Money@Owned >> Unowned mergeFrom) public {
           amount = amount + mergeFrom.amount;
           // disown mergeFrom
       }
   }


Invoking functions
----------------------
 You should check each invocation to make sure it is permitted according to the ownership of the function arguments. For example:

::

   function spend(Money m) public { // acquires ownership of m
      // implementation not shown
   };
   function print(Money m) public { // m is unowned here
       // implementation not shown
   }

   function test() public {
      Money m = ... // assume m is now owned.
      print(m); // m is still owned
      spend(m); // m is now unowned
      spend(m); // Oops, this is a bug! Can't spend(m) because spend() requires an owned parameter.
   }


Handling Errors
-----------------
Errors can be flagged with ``revert``. A description of the error can be provided as well. An example of usage is given below.
::

   function checkAmount(Money m) public { // m is unowned
     if (m.getAmount() < 0) {
         revert("Money must have an amount greater than 0");
     }
   }

