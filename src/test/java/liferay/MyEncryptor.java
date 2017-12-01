package liferay;

import java.security.Key;
import java.security.SecureRandom;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.spec.SecretKeySpec;

//import com.liferay.petra.encryptor.Encryptor;
import com.liferay.portal.kernel.util.Digester;
import com.liferay.portal.kernel.util.StringPool;
import com.liferay.portal.kernel.util.Base64;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class MyEncryptor {

	public static final String ENCODING = Digester.ENCODING;
	public static int KEY_SIZE = 128;
	public static String KEY_ALGORITHM = "AES";
	
	private static final Map<String, Cipher> _encryptCipherMap = new ConcurrentHashMap<String,Cipher>(1, 1F, 1);
	
	public static Key deserializeKey(String base64String) {
		byte[] bytes = Base64.decode(base64String);
		return new SecretKeySpec(bytes, KEY_ALGORITHM);
	}
	
	public static byte[] encryptUnencoded(Key key, byte[] plainBytes) {

			String algorithm = key.getAlgorithm();
			String cacheKey = algorithm.concat(StringPool.POUND).concat(key.toString());
			Cipher cipher = _encryptCipherMap.get(cacheKey);

			try {
				
				if (cipher == null) {
					cipher = Cipher.getInstance(algorithm);
					cipher.init(Cipher.ENCRYPT_MODE, key);
					_encryptCipherMap.put(cacheKey, cipher);
				}

				synchronized (cipher) {
					return cipher.doFinal(plainBytes);
				}
			}
			catch (Exception e) {
				System.err.println(e.getMessage());
			}
			return null;
	}
	
	public static String encrypt(Key key, String plainText) throws Exception {
			if (key == null) {
				return plainText;
			}
			byte[] decryptedBytes = plainText.getBytes(ENCODING);
			byte[] encryptedBytes = encryptUnencoded(key, decryptedBytes);
			return Base64.encode(encryptedBytes);
	}
	
	public static String encrypt(String key, String userId) {
		try {
			return encrypt(deserializeKey(key),userId);
		} catch (Exception e) {
			System.err.println(e.getMessage());
		}
		return null;
	}
	
}