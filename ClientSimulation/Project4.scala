package ClientSimulation

import akka.actor.{ Actor, ActorRef, Props, ActorSystem,ActorSelection,ActorLogging}
import scala.concurrent.duration._
import akka.util.Timeout


object Project4{
	 def main(args: Array[String]) {
		var numOfActiveClient=5000
		var numOfDailyClient=2500
		var numOfMonthlyClient=1500
		var numOfUnactiveClient=1000
		val clientSystem = ActorSystem("ClientSystem")

		//clientSystem.actorOf(Props(new client("Client1", 18, Duration(50, MILLISECONDS))),"Client1")
		for(i <-1 to 100){
			// create active user with age between 18-24, they behave ever 50 millis
			clientSystem.actorOf(Props(new client("Client"+i.toString, 18, Duration(50, MILLISECONDS))),"Client"+i.toString)
			Thread.sleep(10)
		}

		/*for(i <-numOfActiveClient+1 to numOfActiveClient+numOfDailyClient){
			// create active user with age between 25-31, they behave ever 100 millis
			clientSystem.actorOf(Props(new client("Client"+i.toString, 25, Duration(100, MILLISECONDS))), "Client"+i.toString)
			Thread.sleep(10)
		}

		for(i <-numOfActiveClient+numOfDailyClient+1 to numOfActiveClient+numOfDailyClient+numOfMonthlyClient){
			// create active user with age between 11-17, they behave ever 200 millis
			clientSystem.actorOf(Props(new client("Client"+i.toString, 11, Duration(200, MILLISECONDS))), "Client"+i.toString)
			Thread.sleep(10)
		}

		for(i <-numOfActiveClient+numOfDailyClient+numOfMonthlyClient+1 to 10000){
			// create active user with age between 32-28, they behave ever 200 millis
			clientSystem.actorOf(Props(new client("Client"+i.toString, 32, Duration(500, MILLISECONDS))), "Client"+i.toString)
			Thread.sleep(10)
		}*/

		// start all client to send request to REST FACEBOOK server		
		///Thread.sleep(10000)
		//for(i<-1 to 10000) {
			//clientSystem.actorSelection("/user/Client"+i.toString) ! AssignFriends
			//Thread.sleep(10)
		//}

		//Thread.sleep(50000)
		//for(i<-1 to 10000) {
			//clientSystem.actorSelection("/user/Client1") ! Start
		//}		
	}
}
