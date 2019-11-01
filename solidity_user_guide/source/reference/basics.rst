Solidity Language Basics
==========================

Contracts, functions, and main contracts
----------------------------------------------
Solidity is object-oriented. A ``contract`` is like a class: it can be instantiated as many times as needed. Each contract supports operations; each one is called a ``function``. Functions are akin to methods in traditional object-oriented languages. However, unlike methods, functions either completely finish or revert. If a function reverts (via the ``revert`` function), then all changes that the function made will be discarded.

Visibility
--------------
Constructors, functions, etc. must specify *visibility*: `public`, `external`, `internal`, or `private`. For this guide, `public` suffices; it means that the function can be called both within the contract and from other contracts.

Constructors
------------
Constructors must initialize all the fields of their contracts. Constructors are defined with the ``constructor`` keyword. For example:

::

   pragma solidity ^0.5.1;
   
   contract LightSwitch {
      enum SwitchState {Off, On}

      SwitchState state;

      constructor() public {
         state = SwitchState.Off;
      }
   }
