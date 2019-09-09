Using Obsidian on a Blockchain
==============================

This tutorial will not try to teach you about blockchains; there are plenty of other tutorials for that. For now, you only need to know that blockchains are server-based systems that maintain state safely. The blockchain system processes transaction invocations, which come from clients. Your focus so far has been on implementing applications that can run on servers. 

To run transactions, then, we need both a client and the blockchain:

   #. First, instantiate a contract on a blockchain.
   #. Then, clients can invoke transactions that were defined by the contract.

To instantiate a Obsidian contract directly on the blockchain, it must be a `main` contract. Because every client must have a reference to the contract, all the contract's transactions *must* have `this` be a `Shared` reference. But what if the contract is an `asset`? As a special exception to the usual rules, the *blockchain itself* is considered the owner of the instance for the purpose of avoiding asset loss. For example:

::

   main asset contract Bank {
      Money@Owned vault;

      transaction deposit(Bank@Shared this, Money@Owned >> Unowned money) {
         // put money in the vault...
         // (code not shown)
      }
   }

If the `Bank` contract is instantiated on the blockchain, the blockchain itself ensures that the Bank is never lost, and clients can execute transactions via their `Shared` references to the `Bank`.

If you need to understand how clients work, you can refer to :doc:`../reference/clients`, but that is not necessary on your first read of this tutorial.