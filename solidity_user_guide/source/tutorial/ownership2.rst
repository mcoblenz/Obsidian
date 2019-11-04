Ownership -- Functions
=============================================================

  
Function return types
------------------------

When functions return objects, the type of the returned object must be annotated in the function declaration. If the returned reference is owned, a comment should indicate that. For example:

::

    // returns owned Money 
    function withdraw() public returns (Money) {
      // body not shown
      return m; //where m is of type Money
   }



Function parameters
------------------------
When a reference is passed to a function as an argument, the function may expect that ownership of the argument is transferred to the function. A comment in the function declaration can specify what should happen. For example:

::

   function spend(Money m) public { // m is owned initially but must be unowned at the end.
      // implementation not shown
   };

   function testSpend(Money m) public { // m is owned initially but must be Unowned at the end.
      spend(m);
      // spend() takes ownership of m, fulfilling testSpend's obligation to take ownership and transfer it elsewhere. 
   }

If a function expects an argument that is unowned, this means that the function cannot take ownership. 
As a result, it is safe to pass an owned reference as an argument to a function that expects an unowned argument. 
After the function returns, the caller still holds ownership.

 
For example, ``function bar(Money m) // m is unowned`` can accept a 
``Money`` reference with any ownership and the caller maintains whatever ownership it had initially when it called that function.


Function receivers (``this``)
---------------------------------
Sometimes the ownership of ``this`` (the ownership of this contract) needs to change in a function. 
That can be specified by adding  a comment regarding ``this``. For example:

::

   contract Money {
      function discard() public { // this is initially owned but is unowned afterward
         // throws away ownership of this; no code is required here, but the comment shows what is happening
      }
   }

   contract Wallet {
      function throwAwayMoney(Money money) public { // money is initially owned but is unowned afterward
         money.discard();
      }
   }
