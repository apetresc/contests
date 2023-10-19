import java.util.Random;

public class Cipher {
    private final String key;

    public Cipher() {
        key = generateKey(100);
    }

    private String generateKey(int length) {
        Random random = new Random();
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            sb.append((char) (random.nextInt(26) + 'a'));
        }
        return sb.toString();
    }

    public Cipher(String key) {
        assert key.matches("^[a-z]+$");
        this.key = key;
    }

    public String getKey() {
        return key;
    }

    public String encode(String plainText) {
        StringBuilder sb = new StringBuilder(plainText.length());
        for (int i = 0; i < plainText.length(); i++) {
            sb.append((char) ('a' + (((plainText.charAt(i) - 'a') + (key.charAt(i % key.length()) - 'a')) % 26)));
        }
        return sb.toString();
    }

    public String decode(String cipherText) {
        StringBuilder sb = new StringBuilder(cipherText.length());
        for (int i = 0; i < cipherText.length(); i++) {
            sb.append((char) ('a' + (26 + cipherText.charAt(i) - key.charAt(i % key.length())) % 26));
        }
        return sb.toString();
    }
}
