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

  def receive = {
    case "reqT" =>
      val inputStream = new ByteArrayInputStream(("Model()\n").getBytes("UTF-8"))
      //reverse måste ha .lineStream_! men med ReqT funkar .!! + " \n"
      val processOutput = (reqT.#<(inputStream)).!!
      println("msg received :" + "reqT")
      out !  processOutput

    case msg: String =>
      val inputStream = new ByteArrayInputStream((msg).getBytes("UTF-8"))
      val processOutput = (reverse.#<(inputStream)).lineStream_!
      println("msg received :" + msg)
      out !  processOutput.head

  }
}
