package ClientSimulation
import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.security.{KeyPair, KeyPairGenerator,NoSuchAlgorithmException, PrivateKey, PublicKey, KeyFactory}
import javax.crypto.Cipher
import java.security.Signature
import java.security.spec.X509EncodedKeySpec
import java.security.spec.PKCS8EncodedKeySpec




class RSAUtil(name:String,keySize:Int){
  val PRIVATE_KEY_FILE = System.getProperty("user.dir")+"/"+name+"/private.key"
  val PUBLIC_KEY_FILE = System.getProperty("user.dir")+"/"+name+"/public.key"
  val SIGN_ALGORITHMS="SHA256WithRSA"
  val ALGORITHM="RSA"
  def generateKey():Array[Byte] ={
    try {
      val keyGen = KeyPairGenerator.getInstance(ALGORITHM)
      keyGen.initialize(keySize)
      val keyPair = keyGen.generateKeyPair()
      val privateKeyFile = new File(PRIVATE_KEY_FILE)
      val publicKeyFile = new File(PUBLIC_KEY_FILE)
      // Create files to store public and private key
      if (privateKeyFile.getParentFile() != null) {  
        privateKeyFile.getParentFile().mkdirs()  
      }
      privateKeyFile.createNewFile()
      if (publicKeyFile.getParentFile() != null) {  
        publicKeyFile.getParentFile().mkdirs()  
      }
      publicKeyFile.createNewFile() 
      // Saving the Public/Private key in a file  
      val publicKeyOS = new ObjectOutputStream(new FileOutputStream(publicKeyFile))
      publicKeyOS.writeObject(keyPair.getPublic())
      publicKeyOS.close()
      // todo encrypt the private key before store it 
      val privateKeyOS = new ObjectOutputStream(new FileOutputStream(privateKeyFile)) 
      privateKeyOS.writeObject(keyPair.getPrivate()) 
      privateKeyOS.close()
      return keyPair.getPublic().getEncoded()       
    } catch {
        case e: Exception => println("exception caught: " + e);null
    }
	}

  def encrypt(plainText:String, pubkey:Array[Byte]):Array[Byte] = {
    try {
      var publicKey = KeyFactory.getInstance(ALGORITHM).generatePublic(new X509EncodedKeySpec(pubkey));
      var cipher = Cipher.getInstance(ALGORITHM)
      cipher.init(Cipher.ENCRYPT_MODE, publicKey)
      var cipherText = cipher.doFinal(plainText.getBytes()) 
      return cipherText    
    } catch {
        case e: Exception => println("exception caught: " + e); null
    }
  
  }

  def decrypt(cipherText:Array[Byte]):Array[Byte] = {
    try {
      val inputStream = new ObjectInputStream(new FileInputStream(PRIVATE_KEY_FILE))
      var key = inputStream.readObject().asInstanceOf[PrivateKey]
      val cipher = Cipher.getInstance(ALGORITHM)
      cipher.init(Cipher.DECRYPT_MODE, key)
      // remove private key as soon as possilbe
      key=null
      return cipher.doFinal(cipherText)
    } catch {
        case e: Exception => println("exception caught: " + e); null
      }
  }

  def sign(content:String):Array[Byte] ={
    try {
      val inputStream = new ObjectInputStream(new FileInputStream(PRIVATE_KEY_FILE))
      var key = inputStream.readObject().asInstanceOf[PrivateKey]
      val signature = Signature.getInstance(SIGN_ALGORITHMS)
      signature.initSign(key)
      signature.update(content.getBytes)
      key =null
      return signature.sign()
    } catch {
        case e: Exception => println("exception caught: " + e); null
    }

  }

  def doCheck(content:String, pubkey:Array[Byte], DS: Array[Byte]):Boolean = {
    try {
      var publicKey = KeyFactory.getInstance(ALGORITHM).generatePublic(new X509EncodedKeySpec(pubkey));
      val signature = Signature.getInstance(SIGN_ALGORITHMS)
      signature.initVerify(publicKey)
      signature.update(content.getBytes)
      return signature.verify(DS)
    } catch {
        case e: Exception => println("exception caught: " + e); false
    }
  }

}
