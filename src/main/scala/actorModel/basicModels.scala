package actorModel

import akka.actor.{Actor, ActorRef, ActorSystem, Props}  

/** First example of an actor system:  ActorA receives a message and reports it. */
object Model1 {
  final case object Message
  class ActorA extends Actor {
    def receive = {
      case Message => println("ActorA received a message.")
    }
  }
}
object Driver1 {
  import Model1._
  val system = ActorSystem()
  val actorA = system.actorOf(Props[ActorA])
  actorA ! Message
  system.terminate()
}

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
  val system = ActorSystem()
  val actorB = system.actorOf(Props[ActorB])
  val actorA = system.actorOf(Props(classOf[ActorA], actorB))
  println("Sending message to ActorA."); actorA ! Message("Hi!!")
  system.terminate()
}

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
object Driver2b {
  import Model2b._
  val system = ActorSystem()
  val actorA = system.actorOf(Props(classOf[MyActor], "A"))
  val actorB = system.actorOf(Props(classOf[MyActor], "B"))
  println("Sending message to A")
  actorA ! Message("Hi!!", Some(actorB))
  system.terminate()
}

/** Third example. System messages B; B tells A that it has received the message. */
object Model3 {
  case class MessageReceived(id: Int)
  case class Message(id: Int, content: String, sender: ActorRef)
  class MyActor extends Actor {
    def receive = {
      case Message(id, content, sender) => 
        sender ! MessageReceived(id)
      case MessageReceived(id) => 
        println(s"Message $id was recieved.")
    }
  }
}
object Driver3 extends App {
  import Model3._
  val system = ActorSystem("Example")
  val a = system.actorOf(Props(classOf[MyActor]))
  val b = system.actorOf(Props(classOf[MyActor]))
  b ! Message(1, "Hi", a)
  system.terminate()
}

/** Fourth Example. */
