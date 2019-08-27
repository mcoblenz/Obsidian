Ownership -- Transactions
=============================================================

  
Transaction return types
------------------------

When transactions return objects, the type of the returned object must be annotated in the transaction declaration. For example:

::

   transaction withdraw() returns Money@Owned {
      // body not shown
      return m; //where m is of type Money@Owned
   }


If we did not return ``m`` within the transaction, or if ``m`` was not of type ``Money@Owned``, we would get an error.



Transaction parameters
------------------------
When a reference is passed to a transaction as an argument, the transaction's declaration specifies initial and final ownership with ``>>``. 
If ``>>`` is not specified for a certain parameter, then the ownership of that parameter *doesn't change*. For example:

::

   transaction spend(Money@Owned >> Unowned m) { // m is Owned initially but must be Unowned at the end.
      // implementation not shown
   };

   transaction testSpend(Money@Owned >> Unowned m) {
      spend(m);
      // m is now of type Money@Unowned due to specification on spend() declaration.
   }

   transaction foo(Money@Owned m) { //this is the equivalent to Money@Owned >> Owned m
      // body not shown
      
   }

If a transaction expects an argument that is ``Unowned``, this means that the transaction cannot take ownership. 
As a result, it is safe to pass an ``Owned`` reference as an argument to a transaction that expects an ``Unowned`` argument. 
After the transaction returns, the caller still holds ownership.

 
For example, ``transaction bar(Money@Unowned m)`` can accept a 
``Money`` reference with any ownership and the caller maintains whatever ownership it had initially when it called that transaction.


Transaction receivers (``this``)
---------------------------------
Sometimes the ownership of ``this`` (the ownership of this contract) needs to change in a transaction. 
That can be specified by adding ``this`` as the first argument in the transaction declaration. Note that ``this`` is implicit,
and is not an actual parameter. For example:

::

   contract Money {
      transaction discard(Money@Owned >> Unowned this) {
         disown this;
      }
   }

   contract Wallet {
      transaction throwAwayMoney(Money@Owned >> Unowned money) {
         money.discard(); // 'this' argument is implicit; do not include it in transaction call.
      }
   }
