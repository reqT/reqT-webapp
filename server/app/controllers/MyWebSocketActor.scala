package controllers

import java.io.ByteArrayInputStream

import akka.actor._

import scala.sys.process._

object MyWebSocketActor {
  def props(out: ActorRef) = Props(new MyWebSocketActor(out))
}


class MyWebSocketActor(out: ActorRef) extends Actor {
  val reverse = stringToProcess("scala -cp reverse.jar Reverse")
  val reqT = stringToProcess("java -jar reqT.jar")

  def trim(text: String): String = text.drop(text.indexOf("reqT>"))

  def receive = {
    case msg: String =>
      val inputStream = new ByteArrayInputStream((msg + "\n").getBytes("UTF-8"))
      val processOutput = reqT.#<(inputStream).!!
      out !  trim(processOutput)

//    case msg: String =>
//      val inputStream = new ByteArrayInputStream((msg).getBytes("UTF-8"))
//      val processOutput = (reverse.#<(inputStream)).lineStream_!
//      println("msg received :" + msg)
//      out !  processOutput.head

  }
}
