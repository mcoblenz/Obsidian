Ownership -- Introduction
=============================================================


.. highlight:: Obsidian


Principles of ownership
------------------------
Our new programming language is object-oriented. It includes *contracts*, which are like classes, and can have *fields* 
and *transactions*, analogous to Java fields and methods respectively. An Obsidian program must have exactly one ``main contract``.
In addition, of the many variables or fields that reference objects, exactly *one* of them 
can own the object, as seen in diagram *(a)* below. An object can have any number of Unowned references, and, if the object is not Owned, 
it can have any number of Shared references (shown in *(b)* below). An object with Shared references can also have Unowned references,
but not Owned ones.

.. image:: ownership-diagram.png
   :alt: Ownership
   :width: 1000

In other words, the concept of ownership is having different types of references to an object. There are three different 
types of references: ``Owned``, ``Unowned``, and ``Shared``.
Let's use money as an example. If you have $10, that money belongs to you -- you own it. This is the idea of an ``Owned`` reference.
You can show this money to anyone else; they can see the money, and talk about it, but they can't do anything with it -- 
they can't spend it or save it because it's not theirs. This is the idea of an ``Unowned`` reference; it's a reference to an object,
but doesn't have as much manipulative power over the object because it doesn't own the object. Now imagine the $10 is in a public pot that anyone can take from. 
In this case, everyone shares ownership of the money; i.e., you all have ``Shared`` references to it. ``Shared`` references 
are similar to normal references in other programming languages, and are the default ownership type if no permission is specified.


*Note that ownership ONLY applies to objects; primitive types (like ints, strings, booleans, etc.) do NOT have permissions.*


Continuing with money, here is an example of a contract (a ``Wallet``) with an object, a ``Money`` contract, 
that has one ``Owned`` reference:

::

   contract Money {
   }

   main contract Wallet {
      Money @ Owned m; // @Owned indicates that the reference m owns the object it refers to
      
      transaction spendMoney() {
         ...
      }
   }

Note that with this code alone, ``m`` is an ``Owned`` reference that doesn't actually point to any object. If we wanted to create a new object,
we would do it in a similar way to other object-oriented languages: ``m = new Money()``. Now, ``m`` is an Owned reference pointing to a 
``Money`` object.


The compiler tracks ownership of each variable every time the variable is used. This information is part of the *type* of the variable. For example, the type of ``m`` is ``Money@Owned``. Information about ownership is NOT available at runtime; it is only available during compilation.


- If a reference is the only one that holds ownership, then it is ``Owned``.
- If all references to the object are the same (there is no owner), then each reference is ``Shared``.
- If a reference is NOT the owning one, but there might be another owning reference, then the reference is ``Unowned``.
