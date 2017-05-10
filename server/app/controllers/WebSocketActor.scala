package controllers
import java.io.ByteArrayInputStream

import akka.actor._


import scala.sys.process._


object WebSocketActor {
  def props(out: ActorRef) = Props(new WebSocketActor(out))
}



class WebSocketActor(out: ActorRef) extends Actor {
  val reqT = stringToProcess("java -jar reqT.jar")
  //val templateHandler = new TemplateHandler

  def trim(text: String): String = text.drop(text.indexOf("reqT>"))

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def receive = {
    case message: String =>
      val inputStream = new ByteArrayInputStream((message + "\n").getBytes("UTF-8"))
      val processOutput = reqT.#<(inputStream).!!
      out ! trim(processOutput)

    case ReceiveTimeout => println("asdf")
//    case message: String =>
//      message match {
//        case r"Template[1-9][0-9]*" =>
//          out ! templateHandler.getTemplate(message)
//      }
  }
}
