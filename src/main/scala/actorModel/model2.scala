package actorModel

import akka.actor.{Actor, ActorRef}

// For the driver 
import akka.actor.{ActorSystem, Props}

/** Second example. ActorA recieves a message, and passes it to actor B. */
object Model2 {

  final case class Message(content: String)
  class ActorB extends Actor {
    def receive = {
      case Message(content) => println(s"ActorB recieved the message: $content")
    }
  }
  class ActorA(forwardTo: ActorRef) extends Actor {
    def receive = {
      case Message(content) => 
        println("ActorA recieved a message. Passing to ActorB.")
        forwardTo ! Message(content)
    }
  }
}

object Driver2 {
  import Model2._

  val system = ActorSystem("Example")
  val actorB = system.actorOf(Props[ActorB], name="bar")
  val actorA = system.actorOf(Props(classOf[ActorA], actorB), name="foo")
    
  println("Sending message to ActorA.")
  actorA ! Message("Hi!!")  
  system.terminate()
}
