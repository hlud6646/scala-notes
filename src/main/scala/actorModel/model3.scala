package actorModel 

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

/** Third example. System messages B; B tells A that is has received the message.
  */
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

object Driver3 {
  import Model3._
  val system = ActorSystem("Example")
  val a = system.actorOf(Props(classOf[MyActor]))
  val b = system.actorOf(Props(classOf[MyActor]))
  b ! Message(1, "Hi", a)
  system.terminate()
}
