package ClientSimulation
import java.security.SecureRandom
import javax.crypto.Cipher
import javax.crypto.KeyGenerator
import javax.crypto.SecretKey
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

class AESUtil(name:String,keySize:Int){
	val SECRETE_KEY_FILE = System.getProperty("user.dir")+"/"+name
	val rand= new SecureRandom()
	def generateKey(keyName:String) {
		val keyGen = KeyGenerator.getInstance("AES")
		keyGen.init(256,rand)
		val secretKey=keyGen.generateKey()
		val keyFile = new File(SECRETE_KEY_FILE+"/"+keyName+".key")
      	if (keyFile.getParentFile() != null) {  
        	keyFile.getParentFile().mkdirs()  
      	}
      	keyFile.createNewFile()
      	val secreteKeyOS = new ObjectOutputStream(new FileOutputStream(keyFile))
      	secreteKeyOS.writeObject(secretKey)
      	secreteKeyOS.close()
	}

	def returnKey (keyName:String):String ={
     	val inputStream = new ObjectInputStream(new FileInputStream(SECRETE_KEY_FILE+"/"+keyName+".key"))
      	var key = inputStream.readObject().asInstanceOf[SecretKey]
      	return new String(key.getEncoded())		
	}

	def encrypt(plainText:String, keyName:String):(Array[Byte],Array[Byte]) = {
     	val inputStream = new ObjectInputStream(new FileInputStream(SECRETE_KEY_FILE+"/"+keyName+".key"))
      	var key = inputStream.readObject().asInstanceOf[SecretKey]
      	val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
      	val ivBytes = new Array[Byte](16)
      	rand.nextBytes(ivBytes)
      	val IV = new IvParameterSpec(ivBytes)
      	cipher.init(Cipher.ENCRYPT_MODE, key, IV)
      	var cipherText = cipher.doFinal(plainText.getBytes())
      	return (cipherText, ivBytes)		
	}

	def decrypt(cipherText:Array[Byte], key: Array[Byte], ivBytes: Array[Byte]):Array[Byte] = {
		val IV = new IvParameterSpec(ivBytes)
		val originalKey = new SecretKeySpec(key, 0, key.length, "AES")
		val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
		cipher.init(Cipher.DECRYPT_MODE, originalKey, IV)
		return cipher.doFinal(cipherText)
	}
}	