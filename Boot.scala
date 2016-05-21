package FacebookAPI

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import spray.can.Http
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

object Boot extends App {

  // ActorSystem to host our application in
  implicit val system = ActorSystem("FacebookServer")

  // create and start my service actor
  val handler = system.actorOf(Props[MyServiceActor], "serverHandler")

  implicit val timeout = Timeout(15.seconds)
  // start a new HTTP server on port 8080 with my service actor as the handler
  IO(Http) ? Http.Bind(handler, interface = "localhost", port = 8080)
}
