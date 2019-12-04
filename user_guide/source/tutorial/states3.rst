States -- Miscellaneous
========================


Unowned references
-------------------
When there may be an owner of an object, other references cannot be used to modify the state.  These other references are annotated ``Unowned``. For example:

::

   transaction foo(LightSwitch@Unowned s) {
      s.turnOff(); // COMPILE ERROR: can't change state of s through an unowned reference
   }

When Unowned references are used in dynamic state tests, the body of the test does NOT have additional state information. For example:

::

   transaction foo(LightSwitch@Unowned s) {
      if (s in On) {
         s.turnOff(); // STILL GET A COMPILE ERROR: can't change state of s through an unowned reference
      }
   }

If you need to call a transaction that needs the referenced object to be in a particular state, you need to start with at least a ``Shared`` reference (if not an ``Owned`` or state-specifying reference).

Shared references
------------------
If there is no owner of an object, then all references to the object are annotated ``Shared``. These references can be used to change the state of the referenced object, but invoking transactions that can only be called in some states requires a runtime check. For example:

::

   transaction test1(LightSwitch@Shared s) {
      s.turnOn(); // COMPILE ERROR: turnOn requires that s be Off, but s is Shared.
   }

In the above situation, the programmer might need to check the state dynamically with ``if...in``.


Implicit casts
---------------
When a ``Shared`` reference is needed, an ``Owned`` suffices as long as the reference is NOT to an asset. For example, an ``Owned`` reference can be passed as an argument to a transaction that expects a ``Shared`` reference to a non-asset object. However, the caller is left with a ``Shared`` reference.

When an ``Unowned`` reference is needed, any reference suffices, and the caller is left with their original kind of reference.
