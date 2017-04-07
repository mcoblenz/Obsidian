package edu.cmu.cs.obsidian.chaincode;

import java.util.Base64;

/**
 * Created by mcoblenz on 4/6/17.
 */
public class ChaincodeUtils {
    // Utility methods.
    public static boolean bytesRepresentBoolean(byte[] array) {
        return (array.length == 1) && (array[0] == 0 || array[0] == 1);
    }

    public static boolean bytesToBoolean(byte[] array) {
        assert(bytesRepresentBoolean(array));
        return (array[0] == 1);
    }

    private static final byte[] trueArray = new byte[] {1};
    private static final byte[] falseArray = new byte[] {0};

    public static byte[] booleanToBytes(boolean b) {
        return b ? trueArray : falseArray;
    }

    /* Encoding and decoding bytes in a JSON-friendly format is necessary
     * to send them back to the client */
    public static String bytesToJSONString(byte[] bytes) {
        return Base64.getEncoder().encodeToString(bytes);
    }

    public static byte[] JSONStringToBytes(String base64Str) {
        return Base64.getDecoder().decode(base64Str);
    }
}
