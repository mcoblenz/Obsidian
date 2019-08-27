Obsidian Language Basics
==========================

Contracts, transactions, and main contracts
----------------------------------------------
Obsidian is object-oriented. A ``contract`` is like a class: it can be instantiated as many times as needed. Each contract supports operations; each one is called a ``transaction``. Transactions are akin to methods in traditional object-oriented languages. However, unlike methods, transactions either completely finish or revert. If a transaction reverts (via the ``revert`` statement), then all changes that the transaction made will be discarded.

Main contracts
-----------------
A ``main`` contract may be instantiated on the blockchain. The contract's transactions are then available for clients to invoke. Only ``main`` contracts can be deployed directly, and the Obsidian compiler expects every program to have one ``main`` contract.

Constructors
------------
Constructors must initialize all the fields of their contracts. In addition, construtors of contracts that have defined states must transition the object to a particular state. It is good practice for constructors to specify a specific state that the object will be in if possible; otherwise, generally you should declare constructors to return an ``Owned`` reference. For example:

::

   contract LightSwitch {
      state On;
      state Off;

      LightSwitch@Off() { // the resulting object will be in Off state 
         ->Off;
      }
   }
