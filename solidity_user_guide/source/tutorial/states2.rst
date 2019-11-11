States -- Manipulating State
=============================

Changing state
--------------------

Methods, including constructors, can change the state of ``this`` by assignment to the state field:

::

   contract LightSwitch {
      enum State {On, Off}
    
      State state; // this field is always either State.On or State.Off

      constructor() public {
         state = State.Off;
      }
   }

Functions can document what states the object must be in before they can be invoked by using assertions. However, knowledge of state requires ownership. It is the responsibility of the programmer to make sure that such functions are only passed owned references to objects that are in the appopriate state. Functions can use comments to specify what states the object will be in after they exit.

::

   constructor() public { // always ends in Off state
      state = State.Off;
   }

   function turnOn() public // turnOn() can only be called on objects that are in Off state.
   {
      assert(state == State.Off);
      state = State.On;
   }

   function turnOff() public // turnOff() can only be called on objects that are in On state.
   {
      assert(state == State.On);
      state = State.Off;
   }


If a state has fields (like the ``On`` state in the definition of a ``LightSwitch`` with ``brightness``), then we should initialize those fields when transitioning:

::

   function turnOn(int b) public // turnOn() can only be called on objects that are in Off state.
   {
      assert(state == State.Off);
      state = State.On;
      brightness = b;
   }

A function can begin or end in multiple possible states. An example is shown below:

::  

   function doSomething() public // assumes the contract is Off, but transitions the contract to either On or Off
   {
      assert(state == State.Off);
      if (...) {
        state = State.On;
      }
      else {
         state = State.Off;
      }
   }


Testing states with ==
---------------------------
::

   function test2(LightSwitch s) public { // s may be in any state
       if (s.state == State.On) { // runtime check to see whether the object referenced by s is in state On
           s.turnOff();
       }
   }

Within the scope of the ``if`` block, it is the programmer's responsibility to make sure that the state specification is never violated. For example:

::

   function test2(LightSwitch s) public { // s may be in any state
       if (s.state == State.On) { // runtime check to see whether the object referenced by s is in state On
            f(); // DANGER: f may change the state of the object referenced by s via another reference.
            s.turnOff(); // Assertion in turnOff() may fail if f() has changed the state inappropriately.
       }
   }

