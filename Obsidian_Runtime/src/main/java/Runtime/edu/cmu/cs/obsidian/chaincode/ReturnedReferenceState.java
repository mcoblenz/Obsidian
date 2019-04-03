package edu.cmu.cs.obsidian.chaincode;


public class ReturnedReferenceState {
    final Class aClass;
    final boolean isOwnedReference;

    public ReturnedReferenceState(Class aClass, boolean isOwnedReference) {
        this.aClass = aClass;
        this.isOwnedReference = isOwnedReference;
    }

    public Class getClassRef() {
        return aClass;
    }

    public boolean getIsOwnedReference() {
        return isOwnedReference;
    }
}