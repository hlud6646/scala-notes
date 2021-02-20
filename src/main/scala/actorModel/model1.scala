package actorModel

import akka.actor.Actor

// For the driver.
import akka.actor.{ActorSystem, Props}

/** First example of an actor system:  ActorA receives a message and reports it. */
object Model1 {
  final case class Message(content: String)

  class ActorA extends Actor {
    def receive = {
      case Message(content) => println(s"ActorA received a message: $content")
    }
  }
}

object Driver {
  import Model1._

  val system = ActorSystem("foo")
  val actorA = system.actorOf(
    Props[ActorA],
    name = "firstActor"
  )
  actorA ! Message("Hi!")
  system.terminate()
}
