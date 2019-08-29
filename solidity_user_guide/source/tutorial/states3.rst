States -- Miscellaneous
========================


Unowned references
-------------------
When there may be an owner of an object, other references should not be used to modify the state. For example:

::

   function foo(LightSwitch s) public { // s is unowned
      s.turnOff(); // Shouldn't change state of s through an unowned reference
   }

Shared references
------------------
If there is no owner of an object, then all references to the object are shared. These references can be used to change the state of the referenced object, but be careful to not call functions that require ownership. For example:

::

   function test1(LightSwitch s) public { // s is shared
      s.turnOn(); // Oops; turnOn required ownership.
   }

In the above situation, the programmer might need to check the state dynamically with ``if``.


Implicit casts
---------------
When a ``shared`` reference is needed, an ``owned`` suffices as long as the reference is NOT to an asset. For example, an ``owned`` reference can be passed as an argument to a function that expects a ``shared`` reference to a non-resource object. However, the caller is left with a ``shared`` reference.

When an ``unowned`` reference is needed, any reference suffices, and the caller is left with their original kind of reference.
