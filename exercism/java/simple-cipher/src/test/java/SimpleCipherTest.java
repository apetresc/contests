import org.junit.Before;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static org.assertj.core.api.Assertions.assertThat;


@RunWith(Enclosed.class)
public class SimpleCipherTest {
    public static class RandomKeyCipher {
        private Cipher cipherWithDefaultKey;

        @Before
        public void setup() {
            cipherWithDefaultKey = new Cipher();
        }

        /**
         * Here we take advantage of the fact that plaintext of "aaa..." doesn't output the key. This is a critical
         * problem with shift ciphers, some characters will always output the key verbatim.
         */
        @Test
        public void cipherCanEncode() {
            String plainText = "aaaaaaaaaa";
            String cipherText = cipherWithDefaultKey.getKey().substring(0, 10);
            assertThat(cipherWithDefaultKey.encode(plainText)).isEqualTo(cipherText);
        }

        @Test
        public void cipherCanDecode() {
            String cipherText = "aaaaaaaaaa";
            assertThat(cipherWithDefaultKey.decode(cipherWithDefaultKey.getKey().substring(0, 10)))
                .isEqualTo(cipherText);
        }

        @Test
        public void cipherIsReversible() {
            String plainText = "abcdefghij";
            assertThat(cipherWithDefaultKey.decode(cipherWithDefaultKey.encode(plainText))).isEqualTo(plainText);
        }

        @Test
        public void keyIsLowercaseLetters() {
            assertThat(cipherWithDefaultKey.getKey()).matches("^[a-z]+$");
        }
    }

    public static class SubstitutionCipher {
        private Cipher cipherWithDefaultKey = new Cipher("abcdefghij");

        @Test
        public void cipherCanEncode() {
            String plainText = "aaaaaaaaaa";
            String cipherText = "abcdefghij";
            assertThat(cipherWithDefaultKey.encode(plainText)).isEqualTo(cipherText);
        }

        @Test
        public void cipherCanDecode() {
            String plainText = "abcdefghij";
            String cipherText = "aaaaaaaaaa";
            assertThat(cipherWithDefaultKey.decode(plainText)).isEqualTo(cipherText);
        }

        @Test
        public void cipherIsReversibleGivenKey() {
            String plainText = "abcdefghij";
            assertThat(cipherWithDefaultKey.decode(cipherWithDefaultKey.encode(plainText))).isEqualTo(plainText);
        }

        @Test
        public void cipherCanDoubleShiftEncode() {
            String plainText = "iamapandabear";
            String cipherText = "qayaeaagaciai";
            assertThat(new Cipher(plainText).encode(plainText)).isEqualTo(cipherText);
        }

        @Test
        public void cipherCanWrapEncode() {
            String plainText = "zzzzzzzzzz";
            String cipherText = "zabcdefghi";
            assertThat(cipherWithDefaultKey.encode(plainText)).isEqualTo(cipherText);
        }

        @Test
        public void cipherCanWrapDecode() {
            String plainText = "zabcdefghi";
            String cipherText = "zzzzzzzzzz";
            assertThat(cipherWithDefaultKey.decode(plainText)).isEqualTo(cipherText);
        }

        @Test
        public void cipherMessageLongerThanKey() {
            String plainText = "iamapandabear";
            String key = "abc";
            String cipherText = "iboaqcnecbfcr";
            assertThat(new Cipher(key).encode(plainText)).isEqualTo(cipherText);
        }
    }
}

