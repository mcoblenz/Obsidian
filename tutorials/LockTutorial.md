# Introduction

Obsidian is a programming language for implementing smart contracts.
It has several features which keep you safe while writing programs, including:

- State-oriented programming that allows developers to explicitly manage states and transitions between states&mdash;in particular, this allows the compiler to ensure operations are performed in the correct state.
- Linear types and ownership, which allow the compiler to protect important data (e.g., money) from vanishing or being duplicated.

This tutorial will focus on the former of these features, showing how states can be used in a simple application.

# A First Example

Let's consider the following *contract*, which is similar to a class in Java, C#, etc.

```obsidian
main contract Lock {
    state Locked {
        string hash;
        string salt;
    }

    state Unlocked;

    Lock@Unlocked() {
        ->Unlocked;
    }
}
```

Similar to how a Java program must have a main class (which contains a main method), Obsidian programs must contain exactly one main contract.
In the example above, this is the `Lock` contract.
This contract will be initialized and will receive all invocation requests.

We can also see that this contract has two *states*: `Locked` and `Unlocked`.

## States

Each contract may contain any number of states (including zero), each of which one abstract state that the contract can be in at any given time.
A contract is **always** in exactly one state.
To enforce this, construtors of contracts that have states **must** *transition* the contract into a state.
We write transitions using the *transition operator*, `->`, followed by the name of the state we wish to transition into.
The constructor must also be annotated with the state that it transitions into.

Both states and contracts may contain fields.
Fields in a contract are available in all states, but fields in a state are only available in that state.
In this example, that means that the `hash` and `salt` fields can only be accessed in the `Locked` state.
Note that it is important to store the hash of password, rather than the password itself; not only is in general a good idea, but it is even more important for smart contracts, because our fields will be stored on the blockchain, making them publicly visible to anyone.

## Transactions

At the moment, the code above will compile, and can be run, but we cannot interact with it.
To do this, we need to add some *transactions*, which are similar to methods in Java.
Our `Lock` starts out unlocked, and we will want some way to lock it, so let's add that first:

```obsidian
transaction lock(Lock@Unlocked >> Locked this, string _hash, string _salt) {
    ->Locked(hash = _hash, salt = _salt);
}
```

Here we see a number of differences between Obsidian and Java:
- The first parameter, `this` has a type with a *transition*.
This type means that we can only call `lock` if `this` is in the `Unlocked` state, and after the transaction completes, we will be in the `Locked` state.
This restriction is important: if we allowed locking in the `Locked` state, anyone could "relock" the lock with an arbitrary password, and then unlock it, circumventing the original password.
- When we transition to a state which has fields, we can set them as part of the transition, similar to how we pass the fields to a constructor when we create a new object.
- The transaction has no return type; you can think of it as defaulting to `void`.

Let's now look at implementing the complementary `unlock` transaction.
We will need to use some hash function to make sure the password is correct.
For this example, we'll use SHA-256, which we can import as follows:

```obsidian
import "Hash.obs"
```

Then we can write `unlock` using the newly imported functionality:

```obsidian
transaction unlock(Lock@Locked >> Unlocked this, string password) {
    if (new SHA256().hash(password, salt) == hash) {
        ->Unlocked;
    } else {
        revert "Incorrect password!";
    }
}
```

A couple notes:
- The use of `revert`: `revert` functions similarly to throwing an exception, with the important difference that it will also abort the transaction, undoing any changes that were made.
- String comparison is done using the `==` operator.

### State Lists

We can implement `unlock` differently:

```obsidian
transaction unlock(Lock@Locked >> (Locked | Unlocked) this, string _password) {
    if (new SHA256().hash(password, salt) == hash) {
        ->Unlocked;
    }
}
```

Note the difference in type on `this` in the transaction declaration.
This type means "this transaction can only be called in the `Locked` state, and `this` will either be `Locked` or `Unlocked` at the end."
`(Locked | Unlocked)` is called a *state list*, and can be useful in most (all?) places that a normal state annotation can be.
State lists are useful in cases in which the transaction has multiple valid possible outcomes.

## Remarks

// Maybe don't need this section

This implementation is somewhat safer and more concise than a similar Java implementation:
- There is no `null` in Obsidian, which allows us to elide some runtime checks.
- States are either statically guaranteed to be correct, or the compiler will insert the necessary runtime checks for us.
- Naming our states and explicitly defining transitions makes our intentions clearer and makes it more difficult to accidentally set flags or otherwise transition states.
- The code can be run on the blockchain as is, with no additional libraries or configuration.

