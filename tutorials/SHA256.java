package edu.cmu.cs.obsidian.stdlib;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Base64;

public class SHA256 {
    public String hash(final String message) {
        final MessageDigest digest = MessageDigest.getInstance("SHA-256");
        return Base64.getEncoder().encodeToString(digest.digest(message.getBytes(StandardCharsets.UTF_8)));
    }

    public String hash(final String message, final String salt) {
        return hash(message + salt);
    }
}

