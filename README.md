Some test contracts are located in resources/tests. The ones with specifications are "simpleVerification.obs" and "simpleVerification2.obs"; the output of the compiler is shown below. If you'd like to run the complier yourself, you can, but you'll need to have IntellJ (with Scala plugin) installed, and you'll probably need to spend some time tweaking paths so everything gets invoked correctly.

The test cases are admittedly contrived and a bit small; this is due to the last-minute nature of our Dafny port of the compiler. The compiler does work for these cases, and Dafny does report verification errors successfully when they are present.

Here are sample outputs, which currently include both the generated Dafny code and also the Dafny output.

====================== SAMPLE OUTPUT: simpleVerification.obs ================================
edu/cmu/cs/obsidian/generated_code/SimpleVerification.java
javac exited with value 0
jar exited with value 0
datatype SimpleVerification_states =   S1(s1: int) | S2(s2: int) | defaultState

class SimpleVerification {
  var state__: SimpleVerification_states
  
  constructor()
  modifies this;
  {
    this.state__ := defaultState;
  }
  
  var x: int
  var y: int
  
  var s1: int
  var s2: int
  method doStuff (a: int, b: bool) 
  modifies this;
  ensures x > 0;
  {
    x := 1;
    other();
    assert x > 0;
    state__ := S1(a);
  }
  
  method other () 
  modifies this;
  requires x > 0;
  ensures x > 0;
  {
    
  }
}
Dafny program verifier version 1.9.8.30829, Copyright (c) 2003-2016, Microsoft.

Dafny program verifier finished with 6 verified, 0 errors
dafny result: 0
=========================== SAMPLE OUTPUT: simpleVerification2.obs ==============================
edu/cmu/cs/obsidian/generated_code/Simple2.java
javac exited with value 0
jar exited with value 0
datatype Simple2_states =   S1(a: int,b: int) | S2(c: int) | defaultState

class Simple2 {
  var state__: Simple2_states
  
  constructor()
  modifies this;
  {
    this.state__ := defaultState;
  }
  
  var x: int
  var y: int
  var a: int
  var b: int
  method t1 () 
  modifies this;
  ensures y == 1;
  {
    y := 1;
    state__ := S2(2);
  }
  
  var c: int
  method t2 () returns (ret___: int) 
  modifies this;
  ensures y >= 0;
  {
    y := 10;
    state__ := S1(1, 2);
    return y;
  }
}
Dafny program verifier version 1.9.8.30829, Copyright (c) 2003-2016, Microsoft.

Dafny program verifier finished with 6 verified, 0 errors
dafny result: 0
[success] Total time: 19 s, completed May 5, 2017 8:48:17 PM

Process finished with exit code 0

