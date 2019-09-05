Obsidian Clients
=================

Client programs, which interface with smart contracts that are installed on the blockchain, can also be written in Obsidian. Client programs must have a transaction called `main` that takes a `remote` reference to the smart contract as an argument. For example:

::

    import "Auction.obs"

    main contract AuctionClient {
        transaction main (remote Auction@Shared auction) {
            ...
        }
    }

Important things to note about the above example:

- The client file imports the file that implements the smart contract that was installed on the blockchain (`Auction.obs`).
- The client has a transaction called `main`.
- The `main` transaction takes a `remote` reference to an `Auction@Shared`. The top-level smart contract is always `Shared` because many client programs can reference it. However, as a special exception to the usual rule that assets must have owners and there must be no shared references to assets, the blockchain itself is considered the owner of the top-level smart contract. Shared references are permitted because otherwise clients would not be able to use it.


Within the client, when transactions are invoked on `remote` objects, the invocations happen over the network, and become individual transactions on the blockchain. If any of these transactions fails, the whole client program will abort. 

When a reference to a local (i.e. not `remote`) object is passed as an argument to a remote transaction, the local object is packed up and a copy is sent to the blockchain. *The local reference will still point to the local version*, even though the blockchain how has a separate copy. If you want to get a reference to the copy on the blockchain, you will need to do so via separate means, e.g. having the transaction return a reference to the new object you are interested in. This part of the design is a work in progress.


