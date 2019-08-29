Ownership -- Variables
=======================

Assignment
----------
Assignment (``=``) transfers ownership. That is, the variable on the left becomes the *new* owner, and the variable on the right loses ownership. For example:

::

   Money m = ...; // m is Owned
   Money newMoney = m; // m is Unowned, and newMoney is Owned


Fields
-------
*Field declarations* can include ownership documentation as comments. These comments indicate that at the END of each function, the permission (ownership status) of the object the field references MUST match the annotation on the field when it was *declared*. For example:
    
::

   contract Wallet {
     Money money; // money is owned

     // exchange removes (and returns) the current contents of the Wallet, replacing it with the input Money.
     function exchange(Money otherMoney) public returns (Money) { // takes ownership of otherMoney; returns an owned reference
         Money temporaryMoney = money;
         money = otherMoney;
         return temporaryMoney;
     }
   }

NOTE: in the above example, ``money`` becomes unowned in the ``exchange`` function when the ownership 
is transferred to ``temporaryMoney``. However, by the end of the function, ``money`` gets ownership of ``otherMoney``,
so its permission at the end of the function still matches its permission where it was declared (as a field).


Local variables
----------------
Local variables (variables declared within functions) can also have ownership specified in comments. For example:

::

   function withdraw() public returns (Money) { // returns an owned reference
       // body not shown
   }

   function spend(Money m) public { // acquires ownership of m
      // body not shown
   }

   function test() public {
       Money m = withdraw(); // m is owned
       spend(m); // m is now unowned
   }

Constructors
-------------
Constructors are specified just like in other object-oriented languages, but they use the ``constructor`` keyword. 

Fields of a contract must be initialized in the constructor. For example:

::

   contract Wallet {
     Money money; // money is an owned reference

     //Constructor, takes ownership of its Money parameter
     constructor(Money initialMoney) public {
         money = initialMoney;
     }
   }
