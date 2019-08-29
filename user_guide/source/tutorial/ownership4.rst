Ownership -- Miscellaneous
============================

Ownership checks
-----------------
Ownership can be documented and checked by writing the ownership in brackets. For example, ``[m@Owned]`` indicates 
    that ``m`` is an owning reference at that particular point in the code. The compiler will generate an error if this 
    might not be the case. For example:


::

   transaction spend(Money@Owned >> Unowned m) { // m is Owned initially but must be Unowned at the end.
       // implementation not shown
   }

   transaction testSpend(Money@Owned >> Unowned  m) {
      spend(m); [m@Unowned]; // OK
      [m@Owned]; // COMPILE ERROR: m is Unowned here
   }

Ownership checks in ``[]`` *never* change ownership; they only document and check what is known.


Getting rid of ownership
--------------------------
If ownership is no longer desired, ``disown`` can be used to relinquish ownership. For example:
::

   contract Money {
       int amount;

       transaction merge(Money@Owned >> Unowned mergeFrom) {
           amount = amount + mergeFrom.amount;
           disown mergeFrom; // We absorbed the value of mergeFrom, so the owner doesn't own it anymore.
       }
   }


Invoking transactions
----------------------
 The compiler checks each invocation to make sure it is permitted according to the ownership of the transaction arguments. For example:

::

   transaction spend(Money@Owned >> Unowned m) {
      // implementation not shown
   };
   transaction print(Money@Unowned m) {
       // implementation not shown
   }

   transaction test() {
      Money m = ... // assume m is now owned.
      print(m); // m is still Owned
      spend(m); // m is now Unowned
      spend(m); // COMPILE ERROR: spend() requires Owned input, but m is Unowned here
   }


Handling Errors
-----------------
Errors can be flagged with ``revert``. A description of the error can be provided as well. An example of usage is given below.
::

   transaction checkAmount(Money@Unowned m) {
     if (m.getAmount() < 0) {
         revert("Money must have an amount greater than 0");
     }
   }

Getting rid of ownership
--------------------------
If ownership is no longer desired, ``disown`` can be used to relinquish ownership. For example:
::

   contract Money {
       int amount;

       transaction merge(Money@Owned >> Unowned mergeFrom) {
           amount = amount + mergeFrom.amount;
           disown mergeFrom; // We absorbed the value of mergeFrom, so the owner doesn't own it anymore.
       }
   }