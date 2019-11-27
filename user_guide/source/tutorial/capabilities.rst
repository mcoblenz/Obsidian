Taking Advantage of Ownership
==============================

In this last section of the tutorial, you will see how you can use ownership in a new way.

Suppose you want to keep track of who is allowed to open a Lock. In a traditional language, you might build data structures to track this:

::

    class AccessControl {
        List<Person> lockOpeners;
    }

    class Lock {
        void open (Person p) {
            if (lockOpeners.contains(p)) {
                // Open the lock...
            }
        }
    }

This requires centralized tracking of all the people who can open locks. If a person with access wants to grant access to someone else, a central data structure update is required. Furthermore, any bugs in the code that manipulates the lockOpeners list can result in security vulnerabilities. So, let's look at a different way to manage access control in Obsidian.

Suppose we have a ``Key`` contract, which can be used to open a ``Lock``:

::

    contract Key {
        int whichLock;

        transaction getLockID() returns whichLock {
            return whichLock;
        }
    }

    contract Lock {
        int lockID;

        transaction open(Key@Owned key) {
            if (key.getLockID() == lockID) {
                // Open the lock...
            }
        }
    }

Only the owner of a ``Key`` can use it to open a ``Lock``. There is no central list of all the ``Key`` s, but there is no way to copy a ``Key`` or change which lock ID it opens. If someone has an ``Owned`` reference to a ``Key``, that's good enough to open the ``Lock``. The compiler makes sure that each key never has more than one owner. Of course, just like in the real world, the owner of a key can transfer ownership of the key to someone else without having to ask the lock for permission.

We can also make ``KeyDestroyingLock`` objects, which "eat" keys when they are used:

::

    contract KeyDestroyingLock {
        int lockID;

        transaction open(Key@Owned >> Unowned key) {
            if (key.getLockID() == lockID) {
                // Open the lock...
            }
            disown key; // destroy the key so it can only be used once
        }
    }

This means that the compiler checks to make sure that the implementation of ``open`` actually takes ownership of the ``Key`` from the caller, avoiding any bugs in the implementation that might allow the caller to keep the key and use it again. By using ownership to track keys, there's no need to rely on updating the access control list correctly.