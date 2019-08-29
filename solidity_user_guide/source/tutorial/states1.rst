States -- Introduction
=======================

In Solidity, contracts can define *enum* s. For example, a ``LightSwitch`` is always  either ``Off`` or ``On`` (never both):

::

   contract LightSwitch {
    enum State {On, Off}
    
    State state; // this field is always either State.On or State.Off
   }

Sometimes, fields should be available only within certain states. To implement this, use assertions. For example: 

::

   contract LightSwitch {
      enum State {On, Off}
    
      State state; // this field is always either State.On or State.Off
      int brightness; // only available in On state

      function getBrightness() public returns (int) {
         assert (state == State.On);
         return brightness;
      }
   }

In the example above, ``brightness`` can only be accessed and used when a ``LightSwitch`` is in the ``On`` state. 

States and Ownership
---------------------
If an object is owned, then we have the convention that only the owner should be able to change the state. That way, the owner will always know what state the object is in. On the other hand, if a object is shared, any shared reference can be used to change the state.


