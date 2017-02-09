public abstract class ChaincodeBase {
    public abstract String run(ChaincodeStub stub, String func, String[] args);
    public abstract String query(ChaincodeStub stub, String func, String[] args);
    public abstract String getChaincodeID();
    public void start(String[] args) {
        /* nothing needs to be done here for local config */
    }
}
