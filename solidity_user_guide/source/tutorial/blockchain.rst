Using Solidity on a Blockchain
========================================

This tutorial will not try to teach you about blockchains; there are plenty of other tutorials for that. For now, you only need to know that blockchains are server-based systems that maintain state safely. The blockchain system processes transaction invocations, which come from clients. Your focus so far has been on implementing applications that can run on servers. 

To run transactions, then, we need both a client and the blockchain:

   #. First, instantiate a contract on a blockchain.
   #. Then, clients can invoke transactions that were defined by the contract.

You can use https://remix.ethereum.org to test your code in a test environment. Click "Solidity"; then click "New File"; then paste the code you want to test. In the sidebar, click the third icon to use the compiler, and click the fourth icon to deploy your contract and invoke transactions. When a contract is deployed, it is given an *address* at which it can receive invocations. In fact, every object reference is actually just an address. 

Built-in types
---------------
* ``uint`` and ``int`` represent unsigned and signed 256-bit integers, respectively.
* The ``address`` type represents addresses on the blockchain.
* Solidity has built-in hash maps. For example, ``mapping(address => bool)`` is a map from ``address`` to ``bool``. The keys must be primitive types (not objects or structs). You can use a map with [] syntax, e.g. ``map[key]`` looks up ``key`` in the hash map ``map``.  There is no way to check to see if a key is in the mapping.
* Dynamically-sized arrays are supported; the length is stored in a ``length`` property. For example: ::

    address[] addresses;
    function foo() {
        if (addresses.length > 0) {
            bar(addresses[0]); // call bar(), passing the 0th element of addresses
        }
    }
* Arrays can be declared with any element type.
* Hash maps and arrays are automatically initialized so that all their elements are 0 (or equivalent according to the contained type).


Enumerated types
-----------------
As in C, use the ``enum`` keyword to define an enumerated type: ::

    enum State { Open, Closed } // defines a type called State
    State state; // This is a field called 'state' of type 'State'
    function foo () {
        if (state == State.Open) {
            ...
        }
    }

Structs
-------------
To define a data structure, use ``struct``: ::

    struct Point {
        int x;
        int y;
    }

Unlike contract instances, structs do not have addresses.

Constructors
------------
Constructors are defined with the ``constructor`` keyword: ::
    
    constructor(/* parameters go here */) public {
        // initialize fields as needed
    }

Visibility
----------
Each function and field can be annotated with ``public``, which indicates that it can be accessed from anywhere, or ``private``, which means that it can only be accessed from within the same contract.

Some functions read fields but never change them. These functions can be marked ``view`` (after the parameter list, as with ``public``).

Assertions
----------
``assert(condition)`` can be used for checking assumptions; the condition should only be false if there is a bug in your code.

``require(condition, "error message")`` is like an assertion, but it is for ensuring valid conditions, e.g. to make sure that functions are only called when appropriate.

Both ``assert`` and ``require`` terminate execution and discard all changes if the conditions are not met.

Money
------
Solidity has a built-in cryptocurrency, known as *ether*. Function invocations can send ether, but ether is not a normal parameter to functions. Instead, ether accompanies invocations in metadata. For now, you don't need to worry about how to do this from outside the blockchain. Functions that can *receive* ether must be annotated ``payable``. The system will automatically track how much money each contract has received (every contract can automatically hold money).

To transfer money to a given address, invoke ``transfer`` on the recipient's address, passing a quantity of money: ::

    recipient.transfer(amount)

If you attempt to transfer more money than your contract has, your code will be terminated.

You might recall that a lot of the prior examples discussed a ``Money`` contract; you can think of this as a special case of some kind of object that you want to keep careful track of, even though you will be tracking ether without using objects directly. 

Messages
---------
When a client invokes a function, the client sends a *message* to the blockchain, specifying the function to call. The function body has access to a special variable ``msg`` that has some useful fields:

    * ``msg.sender`` is the address of whoever sent the message.
    * ``msg.value`` is the amount of ether that the sender included when sending the message.

If you are using the Remix environment (linked above), messages are created automatically; you don't have to do anything special to create them.

Security
----------
*Important*: for security reasons, it is only safe to transfer money to whoever invoked the present function. This leads to the *withdrawal pattern*: if you want to send money to someone else, do not do it directly. Instead, record in a data structure the fact that your contract owes money to a particular address, and provide a ``withdraw`` function that the *recipient* can call to withdraw the money. Typically, you record a mapping from address to an amount you owe, and then ``withdraw`` checks ``msg.sender`` to transfer the right amount of money (e.g. ``msg.sender.transfer(amount)``).

Storage types
-------------
Data can be stored in three possible locations:

    * ``storage`` is a part of the permanent blockchain state. This is where fields are stored.
    * ``memory`` exists only for the duration of a given invocation. Think of this like the traditional heap. 
    * ``calldata`` is used for arguments of functions that are called by clients.

Usually, you don't need to worry about where data is stored. However, parameters of complex types must specify a location. ``memory`` is usually a reasonable choice, but the details are beyond the scope of this tutorial. If you need more information, see https://solidity.readthedocs.io/en/v0.5.12/types.html#data-location.

Automatic getters
------------------
The compiler automatically generates getters for ``public`` fields. For example: ::

    string public foo;
    // Now you can call foo().

The compiler never automatically creates setters.

Concurrency
------------
On the blockchain, all transactions execute sequentially. As a result, you do not have to worry about concurrency in Obsidian; if your transaction is executing, no other transactions are executing concurrently.