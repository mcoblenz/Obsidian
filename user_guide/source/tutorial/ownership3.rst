Ownership -- Variables
=======================

Assignment
----------
Assignment (``=``) transfers ownership. That is, the variable on the left becomes the *new* owner, and the variable on the right loses ownership. For example:

::

   Money m = ...; // m is Owned
   Money newMoney = m; // m is Unowned, and newMoney is Owned
   // Now m is of type Money@Unowned, not Money@Owned.


Fields
-------
*Field declarations* MUST include ownership specifications. 

This means that at the END of each transaction, the permission (ownership status) of the object the field references MUST match the annotation on the field when it was *declared*. For example:
    
::

   contract Wallet {
     Money@Owned money; // Note that this annotation is on a field declaration!

     // exchange removes (and returns) the current contents of the Wallet,
     // replacing it with the input Money.
     transaction exchange(Money@Owned >> Unowned otherMoney) returns Money@Owned {
         Money temporaryMoney = money;
         money = otherMoney;
         return temporaryMoney;
     }
   }

NOTE: in the above example, ``money`` becomes ``Unowned`` in the ``exchange`` transaction when the ownership 
is transferred to ``temporaryMoney``. However, by the end of the transaction, ``money`` gets ownership of ``otherMoney``,
so its permission at the end of the transaction still matches its permission where it was declared (as a field).


Local variables
----------------
Local variables (variables declared within transactions) are declared *WITHOUT* any ownership specified; 
instead, you can optionally write compiler ownership checks ``[]`` to specify their ownership. 
The compiler will output an error if the ownership does not match the check. For example:
::

   transaction withdraw() returns Money@Owned {
       // body not shown
   }

   transaction test() {
       Money m = withdraw(); [m@Owned];
       spend(m); [m@Unowned];
       [m@Owned]; // COMPILE ERROR: m is Unowned here

Constructors
-------------
Constructors are specified just like in other object-oriented languages, but can include a permission. 

Fields of a contract must be initialized in the constructor. For example:

::

   contract Wallet {
     Money@Owned money; // Note that this annotation is on a field declaration!

     //Constructor, takes in a Money parameter
     Wallet@Owned(Money@Owned >> Unowned initialMoney) {
         money = initialMoney;
     }
   }
