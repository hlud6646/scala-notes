package actorModel

import akka.actor.{Actor, ActorRef}

// For the driver 
import akka.actor.{ActorSystem, Props}

/** Second example again, with forwardTo encoded in the message. */
object Model2b {
  final case class Message(content: String, forwardTo: Option[ActorRef])
  class MyActor(var name: String) extends Actor {
    def receive = {
      case Message(content, None)             => println(s"$name got message: $content")
      case Message(content, Some(forwardTo))  => 
        println(s"$name forwarding message.")
        forwardTo ! Message(content, None)
    }
  }
}

object Driver2b extends App {
  import Model2b._
  val system = ActorSystem("Example")
  val actorA = system.actorOf(Props(classOf[MyActor], "A"))
  val actorB = system.actorOf(Props(classOf[MyActor], "B"))
  println("Sending message to A")
  actorA ! Message("Hi!!", Some(actorB))
  system.terminate()
}
