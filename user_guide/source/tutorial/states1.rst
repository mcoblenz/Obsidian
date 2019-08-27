States -- Introduction
=======================

In our new programming language, contracts can define *states*. For example, a ``LightSwitch`` is always  either ``Off`` or ``On`` (never both):

::

   contract LightSwitch {
     state On;
     state Off;
   }

In addition, fields can be specified to be available only within certain states. For example: 

::

   contract LightSwitch {
       state On {
         int brightness;
       }
       state Off;

       int switchLocation available in On, Off;
   }

In the example above, ``brightness`` can only be accessed and used when a ``LightSwitch`` is in the ``On`` state. On the other hand, ``switchLocation`` can be accessed and used in both ``On`` and ``Off``, but couldn't be accessed in any other states if they existed.

States and Ownership
---------------------
Each object can have one reference that statically specifies what state the object is in. For example, 
``LightSwitch@On`` is the type of a variable that refers to a switch that is in ``On`` state.
   
Note that this is an extension of *ownership*: like ownership, one reference is special. The compiler keeps track 
of the possible states an object can be in and makes sure that the specifications are observed. For example:
  
::

   transaction foo() {
      LightSwitch s = new LightSwitch(); //Assume a LightSwitch is in Off upon instantiation
      s.turnOn();
   }
  
The compiler checks transaction invocations to make sure they are safe:
  
::

   transaction foo() {
      LightSwitch s = new LightSwitch(); //Assume a LightSwitch is in Off upon instantiation
      s.turnOff(); // COMPILE ERROR: turnOff() requires that s is On, but here s is Off
   }
  
*NOTE: there is never a need to specify both *ownership* and *state* at the same time; if a field is in any state, it must be Owned, and if Unowned, the field cannot have a state.*

States and Assets
------------------

States can also be declared as ``asset`` s, which means the contract is an asset (see Part 4) only when in that state.
For example, see an alternate definition of ``LightSwitch`` below, in which a ``LightSwitch`` is an  ``asset`` only
when it is turned ``On``. 

::

   contract LightSwitch {
      asset state On;
      state Off;
   }



