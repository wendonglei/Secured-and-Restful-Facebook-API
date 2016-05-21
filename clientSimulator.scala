package ClientSimulation

import scala.concurrent.Await
import scala.util.{Success, Failure}
import akka.actor.{ Actor, ActorRef, Props, ActorSystem,ActorSelection,ActorLogging}
import akka.util.Timeout
import scala.concurrent.duration._
import akka.actor.Scheduler
import scala.util.Random
import akka.io.IO
import akka.pattern.ask
import scala.concurrent.Future
import spray.http._
import spray.client.pipelining._
import spray.httpx.SprayJsonSupport._
import spray.json._
import org.apache.commons.codec.binary.Base64
import java.security.SecureRandom


case object Start
case object API1
case object AssignFriends
case class ReadUser(userID:Long)
case class ReadPost(postID:Long)
case class Addfriend(friendID:Long)
case class ReadPage(pageID:Long)


case class ResponseJson(data:Array[Byte], IV:Array[Byte], secretKey:Array[Byte])
case class AESkeys(postKey:Array[Byte], pageKey:Array[Byte], listKey:Array[Byte])
case class EncryptJson(data:Array[Byte], IV:Array[Byte])
case class User(name: String, birthday: String, pubkey:Array[Byte])                         
case class RegistResult(ID:Long)                          
case class Posts(message:String, place: String)
case class Page(name:String, link:String)      
case class Friendlist(name:String, category:String)
case class JsonResult(data:List[Long])
case class ResgisterResult(ID:Long)



object JsonImplicits extends DefaultJsonProtocol {
	implicit val userFormats = jsonFormat3(User)                        // json for User registration
    implicit val results1 = jsonFormat1(RegistResult)                   // json for response to succcessful registration
   	implicit val postFormats = jsonFormat2(Posts)
   	implicit val pageFormats = jsonFormat2(Page)
   	implicit val ListFormats = jsonFormat2(Friendlist)
   	implicit val JsonResultFormats = jsonFormat1(JsonResult)
    implicit val EncryptFormats = jsonFormat2(EncryptJson)
    implicit val KeysFormats = jsonFormat3(AESkeys)
    implicit val ResponseFormats = jsonFormat3(ResponseJson)
    implicit val RegisterFormats = jsonFormat1(ResgisterResult)    
}


class client(name1:String, ageStart:Int, activeness:FiniteDuration) extends Actor with ActorLogging{
	import JsonImplicits._
  	import context.dispatcher
  	private implicit val timeout: Timeout = 15.seconds
  //val Http.HostConnectorInfo(hostConnector, _) <- IO(Http) ? Http.HostConnectorSetup("localhost", port = 8080)
	/*val pipeline: Future[SendReceive] =
  		for (
    		Http.HostConnectorInfo(connector, _) <-
      		IO(Http) ? Http.HostConnectorSetup("localhost", port = 8080)
  		) yield sendReceive(connector)*/
    val r= new SecureRandom()
    val base64 = new Base64()
    var UID=new Array[Byte](16)
    var ID:Long=0L
    var friendlistID:Long=0L
    //var token:Long=0L
    var span=activeness
    var name=name1
    var birthday:String=birthGenerator(ageStart)
    val RSAtool= new RSAUtil(name,1024)
    var pubkey:Array[Byte]= RSAtool.generateKey()
    val AEStool= new AESUtil(name,256)
    AEStool.generateKey("post")
    AEStool.generateKey("page")
    AEStool.generateKey("friendlist")
    //log.info((User(name,birthday,pubkey).toJson.toString))
    var friends: Array[Long]= null //Array()
    // register the new user when actor start
    val pipeline:HttpRequest => Future[ResgisterResult]= sendReceive ~> unmarshal[ResgisterResult]
    val pipeline1:HttpRequest => Future[User]= sendReceive ~> unmarshal[User]
    val pipeline2:HttpRequest => Future[ResponseJson]= sendReceive ~> unmarshal[ResponseJson]
    //val pipeline3:HttpRequest => Future[Page]= sendReceive ~> unmarshal[Page]
    val pipeline4:HttpRequest => Future[JsonResult]= sendReceive ~> unmarshal[JsonResult]
    var pipeline5= sendReceive


    var response = pipeline(Post("http://localhost:8080/user", User(name,birthday,pubkey)))
    
    response onComplete {
    	case Success(results) =>
        ID=results.ID
        //token=results.token
      	log.info("Register: "+ name+":"+ID)

      case Failure(error) =>
      log.error(error,"failed to register "+name) 
    }

  def receive ={
    case 1  =>        // create new post
      r.nextBytes(UID)// generate random UID through secure random of 2^128
      val request = "http://localhost:8080/user/"+ID+"/post?UID="+base64.encodeToString(UID)
      var DS= RSAtool.sign(request)
      val uri= request+"&DS="+base64.encodeToString(DS)
      var newpost = Posts(strGenerator,strGenerator).toJson.toString
      var (cipherText, ivBytes) = AEStool.encrypt(newpost,"post")
      val response = pipeline5(Post(uri,EncryptJson(cipherText,ivBytes)))
      response onComplete{
        case Success(_) => log.info("User:"+ID+"create post")
        case Failure(error) => log.error(error,"User:"+ID+"failed to create post") 
      } 

    case 2  =>  // create new page
      r.nextBytes(UID)
      val request = "http://localhost:8080/user/"+ID+"/page?UID="+base64.encodeToString(UID)
      var DS= RSAtool.sign(request)
      val uri= request+"&DS="+base64.encodeToString(DS)
      var newpage = Page(strGenerator,strGenerator).toJson.toString
      var (cipherText, ivBytes) = AEStool.encrypt(newpage,"page")
      val response = pipeline5(Post(uri,EncryptJson(cipherText,ivBytes)))
      response onComplete{
        case Success(_) => log.info("User:"+ID+"create page")
        case Failure(error) => log.error(error,"User:"+ID+"failed to create page") 
      }

    case 3 =>   // create a friend list
      r.nextBytes(UID)
      val request = "http://localhost:8080/user/"+ID+"/friendlist?UID="+base64.encodeToString(UID)
      var DS= RSAtool.sign(request)
      val uri= request+"&DS="+base64.encodeToString(DS)
      var newlist = Friendlist(strGenerator,strGenerator).toJson.toString
      var (cipherText, ivBytes) = AEStool.encrypt(newlist,"friendlist")
      val response = pipeline5(Post(uri,EncryptJson(cipherText,ivBytes)))
      response onComplete{
        case Success(_) => log.info(name+" create friendlist")
        case Failure(error) => log.error(error,name+" failed to create friendlist:") 
      }

    case 4 => // read self friendlist id 
      val uri= "http://localhost:8080/user/"+ID+"/friendlist"
      val response = pipeline4(Get(uri))
      response onComplete{
        case Success(results) => log.info("User:"+ID+"read self friendlist ID"); friendlistID=results.data(0)
        case Failure(error) => log.error(error,"User:"+ID+"failed to read metadata") 
      }

    case Addfriend(friendID) =>
      val uri= "http://localhost:8080/getKey/"+friendID
      val response = pipeline5(Get(uri))
      response onComplete{
        case Success(results) => {
          var friendKey=base64.decode(results.toString)
          r.nextBytes(UID)
          val request = "http://localhost:8080/friendlist/"+friendlistID+"/"+ID+"/add/"+friendID+"?UID="+base64.encodeToString(UID)
          var DS= RSAtool.sign(request)
          val uri= request+"&DS="+base64.encodeToString(DS)
          var postKey =RSAtool.encrypt(AEStool.returnKey("post"),friendKey)
          var pageKey =RSAtool.encrypt(AEStool.returnKey("page"),friendKey)
          var listKey =RSAtool.encrypt(AEStool.returnKey("friendlist"),friendKey)
          val response = pipeline5(Post(uri,AESkeys(postKey, pageKey, listKey)))
          response onComplete{
            case Success(_) => log.info("User:"+ID+"add friend succcessfully:"+friendID);
            case Failure(error) => log.error(error,"User:"+ID+"failed to add friend") 
          }
        }

        case Failure(error) => log.error(error,"User:"+ID+"failed to read metadata") 
      }



    case ReadUser(userID)  => // read a user's information
      val uri = "http://localhost:8080/user/"+userID
      val response = pipeline1(Get(uri)) 
      response onComplete{
        case Success(results) => log.info("User:"+ID+"read "+userID+"'s imformation: "+results.name+", "+results.birthday)
        case Failure(error) => log.error(error,"User:"+ID+"failed to read self imformation") 
      }

    case ReadPost(postID)  => //// read a post's information
      r.nextBytes(UID)
      val request = "http://localhost:8080/post/"+postID+"/"+ID+"?UID="+base64.encodeToString(UID)
      var DS= RSAtool.sign(request)
      val uri= request+"&DS="+base64.encodeToString(DS)
      val response = pipeline2(Get(uri))
      response onComplete{
        case Success(results) => 
        val orginalKey = RSAtool.decrypt(results.secretKey)
        val postInfo = AEStool.decrypt(results.data, orginalKey,results.IV)
        log.info("User:"+ID+"read post:"+postID+"'s imformation: "+postInfo)
        case Failure(error) => log.error(error,"User:"+ID+"failed to read post's imformation") 
      }


    case ReadPage(pageID)  => //// read a page's information
      r.nextBytes(UID)
      val request = "http://localhost:8080/page/"+pageID+"/"+ID+"?UID="+base64.encodeToString(UID)
      var DS= RSAtool.sign(request)
      val uri= request+"&DS="+base64.encodeToString(DS)
      val response = pipeline2(Get(uri))
      response onComplete{
        case Success(results) => 
        val orginalKey = RSAtool.decrypt(results.secretKey)
        val pageInfo = AEStool.decrypt(results.data, orginalKey,results.IV)
        log.info("User:"+ID+"read page:"+pageID+"'s imformation: "+pageInfo)
        case Failure(error) => log.error(error,"User:"+ID+"failed to read page's imformation") 
      }      

  }

	

  def strGenerator():String ={
    return new Random().alphanumeric.take(8).mkString
  }

  def birthGenerator(ageRange:Int):String ={
    var year = r.nextInt(7)+2015-ageRange
    var month = r.nextInt(12)+1
    var day = r.nextInt(30)+1

    return ""+month+"/"+day+"/"+year
  }  

}