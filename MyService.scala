package FacebookAPI
import ClientSimulation.RSAUtil
import akka.actor.{Actor, ActorLogging}
import akka.event.Logging
import spray.routing._
import spray.http._
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Success, Failure}
import scala.collection.mutable.{Map,ListBuffer,Set}
import scala.util.Random
import scala.math._
import akka.pattern.ask
import akka.util.Timeout
import spray.httpx.SprayJsonSupport.sprayJsonMarshaller
import spray.httpx.SprayJsonSupport.sprayJsonUnmarshaller
import spray.json.DefaultJsonProtocol
import spray.routing.AuthenticationFailedRejection._
import spray.routing.authentication.{Authentication, ContextAuthenticator}
import java.security.PublicKey
import org.apache.commons.codec.binary.Base64


  case class AESkeys(postKey:Array[Byte], pageKey:Array[Byte], listKey:Array[Byte])
  case class EncryptJson(data:Array[Byte], IV:Array[Byte])
  case class ResponseJson(data:Array[Byte], IV:Array[Byte], secretKey:Array[Byte])
  case class User(name: String, birthday: String, pubkey:Array[Byte])                                                  
  case class JsonResult(data:List[Long])
  case class ResgisterResult(ID:Long) 

  object JsonImplicits extends DefaultJsonProtocol {
    implicit val userFormats = jsonFormat3(User)                        // json for User registration
    implicit val JsonResultFormats = jsonFormat1(JsonResult)
    implicit val EncryptFormats = jsonFormat2(EncryptJson)
    implicit val KeysFormats = jsonFormat3(AESkeys)
    implicit val ResponseFormats = jsonFormat3(ResponseJson)
    implicit val RegisterFormats = jsonFormat1(ResgisterResult)
  }


class MyServiceActor extends Actor with HttpService with ActorLogging {
  //for actor ask and 
  implicit val timeout: Timeout = 15.second
  import context.dispatcher
  def actorRefFactory = context

  def receive = runRoute(userRoute~postRoute~pageRoute~friendlistRoute)

  val r = new Random()// random generator
  val RSAtool= new RSAUtil("server",1024)
  val base64 = new Base64()

  var userDB= Map[Long,User]()
  //var pubKeyDB= Map[Long,Array[Byte]]()
  var pageDB= Map[Long,EncryptJson]()
  var postDB= Map[Long,EncryptJson]()
  var friendlistDB= Map[Long,EncryptJson]()

  var user_postDB= Map[Long,ListBuffer[Long]]()
  var user_pageDB= Map[Long,ListBuffer[Long]]()
  var user_friendlistDB= Map[Long,ListBuffer[Long]]()

  var post_userDB = Map[Long,Long]()
  var page_userDB = Map[Long,Long]()

  var access_keysDB = Map[Long,Map[Long,AESkeys]]()

  var user_page_likeDB= Map[Long,ListBuffer[Long]]()  
  var post_user_likeDB= Map[Long,ListBuffer[Long]]()
  var friendlist_membersDB= Map[Long,ListBuffer[Long]]()  
  var searchDB= Map[String,Long]()
  //var tokenDB=Map[Long,Long]()
  var UID_DB=ListBuffer[String]()




  import JsonImplicits._

  val userRoute = {
    path("user") {
      post {
        entity(as[User]) { newUser =>
          complete(StatusCodes.Created, register(newUser))
        }
      } ~
      get {
        parameters('name) { name => {
            searchDB.get(name) match {
              case Some(userID) => complete(userDB.get(userID))
              case None =>  complete(StatusCodes.NotFound, "Oh man, the person you are looking for not NotFound .")
            }
          }
        }
      }
    }~
    pathPrefix("user" / LongNumber) { userID=>
      post {
        requestUri { uri =>
          var request= uri.toString.split("&")(0)
          parameters('UID,'DS) { (UID,DS) => 
            authenticate(validate(UID,userID,DS,request)) { trueID=>
              pathEnd {
                entity(as[User]) { newUser =>         //update an existing user
                  update(trueID,newUser)
                  complete(StatusCodes.OK, "Succcess")
                }                             
              }~
              path("page") {
                entity(as[EncryptJson]) { newPage =>        //this User creates a new page and serves as admin
                  createPage(trueID,newPage)
                  complete(StatusCodes.Created)
                }                
              }~
              path("post") { 
                entity(as[EncryptJson]) { newPost =>       // this User creates a new post 
                  createPost(trueID,newPost)
                  complete(StatusCodes.Created)
                }               
              }~
              path("friendlist") {
                entity(as[EncryptJson]) { newList =>  // this User creates a new friendlist 
                  createList(trueID,newList)
                  complete(StatusCodes.Created)
                }
              }              
            }
          }
        }        
      }~
      get {
        pathEnd {
          userDB.get(userID) match {      // reading the requested user content
            case Some(aUser) => complete(aUser)
            case None => complete(StatusCodes.NotFound, "Not Found")
          }
        }~
        path("accounts") {              // reading Facebook Pages ID this person administers/is an admin for
          user_pageDB.get(userID) match {
            case Some(buffer) => complete(JsonResult(buffer.toList))
            case None => complete(StatusCodes.NotFound, "Not Found")
          }           
        }~
        path("likes") {               // reading all the Pages ID this person has liked
          user_page_likeDB.get(userID) match {
            case Some(buffer) => complete(JsonResult(buffer.toList))
            case None => complete(StatusCodes.NotFound, "Not Found")
          }
        }~
        path("posts") {               // reading all the user's posts ID
          user_postDB.get(userID) match { 
            case Some(buffer) => complete(JsonResult(buffer.toList))
            case None => complete(StatusCodes.NotFound, "Not Found")          
          }
        }~ 
        path("friendlist") {          // reading the user's friendlists ID
          user_friendlistDB.get(userID) match {
            case Some(buffer) => complete(JsonResult(buffer.toList))
            case None => complete(StatusCodes.NotFound, "Not Found")
          }
        }
      }
    }
  }


  val postRoute = {
    path("post" / LongNumber / LongNumber) { (postId, userID)=>
      requestUri { uri =>
        var request= uri.toString.split("&")(0)
        parameters('UID,'DS) { (UID,DS) =>   
          authenticate(validate(UID,userID,DS,request)) { trueID=>
            get {
              post_userDB.get(postId) match {
                case Some(owenerID) => 
                  if(access_keysDB isDefinedAt trueID) {
                    var keys=access_keysDB(trueID)
                    if(keys isDefinedAt owenerID){
                      var key=keys(owenerID).postKey
                      var post= postDB(postId)
                      complete(ResponseJson(post.data, post.IV , key))
                    }else complete(StatusCodes.NotFound, "Not Found")
                  }else complete(StatusCodes.NotFound, "Not Found")

                case None => complete(StatusCodes.NotFound, "Not Found")
              }
            }~
            post {  // update post
              entity(as[EncryptJson]) { newPost => 
                post_userDB.get(postId) match {
                  case Some(trueID) => postDB(postId)=newPost 
                                       complete(StatusCodes.OK, "succcessful")
                  case None => complete(StatusCodes.NotFound, "resouce is not existing")
                }
              }
            }~
            delete {
              postDB -= postId
              complete(StatusCodes.OK, "Succcess")
            }            
          }
        }     
      }
    }~
    path("post" / LongNumber / "likes") { postId =>   // all user liked this post
      get {
        post_user_likeDB.get(postId) match {
          case Some(buffer) => complete(JsonResult(buffer.toList))
          case None => complete(StatusCodes.NotFound, "Not Found")
        }
      }   
    }~
    path("post" / LongNumber / "likes"/ LongNumber) { (postId,userID) =>  // user like a post
      requestUri { uri =>
        var request= uri.toString.split("&")(0)
        parameters('UID,'DS) { (UID,DS) =>   
          authenticate(validate(UID,userID,DS,request)) { trueID=>
            post {
              insertDB(post_user_likeDB,postId,trueID)
              complete("Succcess")              
            }~
            delete {
              deleteDB(post_user_likeDB,postId,trueID)
              complete("Succcess")
            }
          }
        }
      }
    }
  }


  val pageRoute = {
    path("page" / LongNumber / LongNumber) { (pageId, userID)=>
      requestUri { uri =>
        var request= uri.toString.split("&")(0)
        parameters('UID,'DS) { (UID,DS) =>   
          authenticate(validate(UID,userID,DS,request)) { trueID=>
            get {
              page_userDB.get(pageId) match {
                case Some(owenerID) => 
                  if(access_keysDB isDefinedAt trueID) {
                    var keys=access_keysDB(trueID)
                    if(keys isDefinedAt owenerID){
                      var key=keys(owenerID).pageKey
                      var page= pageDB(pageId)
                      complete(ResponseJson(page.data, page.IV , key))
                    }else complete(StatusCodes.NotFound, "Not Found")
                  }else complete(StatusCodes.NotFound, "Not Found")

                case None => complete(StatusCodes.NotFound, "Not Found")
              }
            }~
            post {  // update post
              entity(as[EncryptJson]) { newPage => 
                page_userDB.get(pageId) match {
                  case Some(trueID) => pageDB(pageId)=newPage 
                                       complete(StatusCodes.OK, "succcessful")
                  case None => complete(StatusCodes.NotFound, "resouce is not existing")
                }
              }
            }~
            delete {
              pageDB -= pageId
              complete(StatusCodes.OK, "Succcess")
            }            
          }
        }     
      }
    }~
    path("page" / LongNumber / "likes"/ LongNumber) { (pageId,userID) =>  // user like a page
      requestUri { uri =>
        var request= uri.toString.split("&")(0)
        parameters('UID,'DS) { (UID,DS) =>   
          authenticate(validate(UID,userID,DS,request)) { trueID=>
            post {
              insertDB(user_page_likeDB,trueID,pageId)
              complete("Succcess")              
            }~
            delete {
              deleteDB(user_page_likeDB,trueID,pageId)
              complete("Succcess")
            }
          }
        }
      }
    }
  }

  val friendlistRoute = {
    path("friendlist" / LongNumber) { listId =>
      get {
        complete(friendlistDB.get(listId))
      }
    }~
    path("friendlist" / LongNumber / "members") { listId =>
      get {
        friendlist_membersDB.get(listId) match {
          case Some(buffer) => complete(JsonResult(buffer.toList))
          case None => complete(StatusCodes.NotFound, "Not Found")
        }        
        //complete(JsonResult(friendlist_membersDB.getOrElse(listId,List())))
      }
    }~
    path("friendlist" / LongNumber / LongNumber/ "add" /LongNumber) { (listID, localID, friendID) =>
      post {
        requestUri { uri =>
          var request= uri.toString.split("&")(0)
          parameters('UID,'DS) { (UID,DS) => 
            authenticate(validate(UID,localID,DS,request)) { trueID=>
              authorize(user_friendlistDB(trueID).contains(listID)) {
                entity(as[AESkeys]) { keys =>
                  grandKeys(access_keysDB,friendID,trueID->keys)
                  insertDB(friendlist_membersDB,listID,friendID)
                  complete("Succcess")
                }                
              }
            }
          }
        }
      }
    }~
    path("getKey"/ LongNumber) { userID =>
      get {
        userDB.get(userID) match {
            case Some(user) => complete(base64.encodeToString(user.pubkey))
            case None => complete(StatusCodes.NotFound, "Not Found")          
        }
      }
    }
  }

  def randomID(DB:Map[Long, _]):Long= {
    var ID = math.abs(r.nextLong());
    while(DB.contains(ID)) {
      ID = math.abs(r.nextLong());
    }
    return ID    
  }
  def update(id:Long, newone:User)= {
    userDB(id)=newone
    log.info("User:"+id+" updated")
  }

  def insertDB(DB:Map[Long, ListBuffer[Long]], id:Long, node:Long) {
    if(DB isDefinedAt id){
      DB(id)+=node
    }else
      DB +=(id->ListBuffer(node))    
  }

  def grandKeys(DB:Map[Long, Map[Long,AESkeys]], id:Long, node:(Long,AESkeys)) {
    if(DB isDefinedAt id){
      DB(id)+=node
    }else
      DB +=(id->Map(node))    
  }  

  def deleteDB(DB:Map[Long, ListBuffer[Long]], id:Long, node:Long) {
    if(DB isDefinedAt id){
      DB(id)-=node
    } 
  }

  def register(newUser:User):ResgisterResult = {
    //var id = BigInt(UUID.randomUUID().toString().replaceAll("-", ""), 16)
    var ID = randomID(userDB)
    //var token = randomID(tokenDB)
    userDB += (ID->newUser)
    //pubKeyDB+=(ID->newUser.pubKey)
    //searchDB += (newUser.name->ID)
    //tokenDB +=(token->ID)
    log.info("Create a new User with ID: "+ID)
    return ResgisterResult(ID)
  }

  def createPage(userID:Long, newPage: EncryptJson) {
    var ID = randomID(pageDB)
    pageDB +=(ID->newPage)
    insertDB(user_pageDB,userID,ID)
    page_userDB+=(ID->userID)
    log.info("UserID: "+userID+" Create a new Page with ID: "+ID)
  }

  def createPost(userID:Long, newPost: EncryptJson) {
    var ID = randomID(postDB)
    postDB +=(ID->newPost)
    insertDB(user_postDB,userID,ID)
    post_userDB+=(ID->userID)
    log.info("UserID: "+userID+" Create a new Post with ID: "+ID)
  }

  def createList(userID:Long, newList: EncryptJson) {
    var ID = randomID(friendlistDB)
    friendlistDB +=(ID->newList)
    insertDB(user_friendlistDB,userID,ID)
    //friendlist_membersDB+=(ID->genFriendlist())
    log.info("UserID: "+userID+" Create a new friendlist with ID: "+ID)
  }

  /*def genFriendlist():List[Long]={
    if(userDB.size <= 100){
      return userDB.keys.toList
    }else {
      var result = Set[Long]()
      var base = userDB.keys.toArray
      var control=r.nextInt(50)+50
      while(result.size < control){
        var value = base(r.nextInt(base.length))
        result+=value
      }
      //log.info("size:"+result.size)
      return result.toList
    }
  } */

  def validate(UID: String, userID:Long, DS: String, content:String): Future[Authentication[Long]] = {
    if(UID_DB.contains(UID))
        return Future{Left(AuthenticationFailedRejection(CredentialsRejected, List()))}
    var key = userDB(userID).pubkey
    var dsBytes = base64.decode(DS)
    val pass = RSAtool.doCheck(content,key,dsBytes)
    if(pass){
      UID_DB+=UID
      return Future {Right(userID)}
    }
    else
      return Future {Left(AuthenticationFailedRejection(CredentialsRejected, List()))}
  }  

}
