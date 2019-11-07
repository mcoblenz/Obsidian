States -- Manipulating State
=============================

The ``->`` Operator
--------------------

Methods, including constructors, can change the state of ``this`` with the ``->`` operator:

::

   LightSwitch() { // constructor
      ->Off; // Transition to Off state.
   }

Methods can specify what states the object must be in before they can be invoked and what states the object will be in after they exit by annotating the ``this`` parameter. Constructors can specify what state they end in.

::

   LightSwitch@Off() { // constructor always ends with the object in Off state
      ->Off;
   }

   transaction turnOn(LightSwitch@Off >> On this) // turnOn() can only be called on objects that are in Off state.
   {
      ->On;
   }

   transaction turnOff(LightSwitch@On >> Off this)
   {
      ->Off;
   }


If a state has fields (like the ``On`` state in the definition of a ``LightSwitch`` with ``brightness``), then we can
transition to that state as follows:

::

   transaction turnOn(LightSwitch@Off >> On this, int b) // turnOn() can only be called on objects that are in Off state.
   {
    ->On (brightness = b); // initializes the brightness field to b
   }

In addition, a transaction can begin or end in multiple possible states. This is specified with the ``|`` operator. An example is shown below:

::  

   transaction doSomething(LightSwitch@Off >> (On | Off) this)
   {
      if (...) {
         -> On;
      }
      else {
         -> Off;
      }
   }

Alternative field initialization
--------------------------------
Rather than initializing fields at the moment of the transition (e.g., ``->On (brightness = b)``), you can initialize fields before transitioning: ::

    transaction turnOn(LightSwitch@Off >> On this, int b) // turnOn() can only be called on objects that are in Off state.
    {
        On::brightness = b; // prepare to initialize field in the next transition to On
        ->On;
    }

Optional compiler checks
-------------------------
As before, the programmer can use ``[]`` statements to check state information at compile time. For example, ``[s@Off];`` will cause a compiler error if ``s`` does not refer to an object in ``Off`` state.


Testing states with ``in``
---------------------------
::

   transaction test2(LightSwitch@Shared s) {
       if (s in On) { // runtime check to see whether the object referenced by s is in state On
           s.turnOff(); // OK due to runtime check
       }
   }

Within the scope of the ``if...in`` block, the compiler requires that if there *is* an owner of the object referenced by ``s``, then the owner's state specification is never violated. If it is, then the program is terminated; it is up to the programmer to make sure the body of the ``if in`` block does not change the state inappropriately.

For now, tests like ``s in On`` cannot be used like general Boolean expressions. If you need to test more than one condition (with `&&`, etc.), you will need to nest `if` statements instead.
