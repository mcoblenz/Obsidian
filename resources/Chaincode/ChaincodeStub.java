import java.util.HashMap;
import java.util.Map;

/* This stub allows testing of code off the blockchain by handling state
 * queries locally*/

public class ChaincodeStub {
    private final String id;
    private final Map<String, String> state;

    public ChaincodeStub(String id) {
        this.id = id;
        this.state = new HashMap<String, String>();
    }

    public String getUuid() { return id; }

    public String getState(String key) {
        if (state.containsKey(key)) {
            return state.get(key);
        } else {
            return "";
        }
    }

    public void putState(String key, String value) {
        state.put(key, value);
    }

    public void delState(String key) {
        state.remove(key);
    }

    public void invokeChaincode(String name, String func, String[] args) {}

}
