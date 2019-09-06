States and Assets
------------------

States can also be declared as ``asset`` s, which means the contract is an asset (see Part 4) only when in that state.
For example, see an alternate definition of ``Wallet`` below, in which a ``Wallet`` is an  ``asset`` only
when it is ``Full``. 

::

   contract Wallet {
      asset state Full;
      state Empty;
   }

