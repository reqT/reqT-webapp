package controllers

import java.io.ByteArrayInputStream
import akka.actor._
import scala.sys.process._
import scala.io.Source

object WebSocketActor {
  def props(out: ActorRef) = Props(new WebSocketActor(out))
}



class WebSocketActor(out: ActorRef) extends Actor {
//  val reqT = stringToProcess("java -jar reqT.jar")
      val sysRuntime = Runtime.getRuntime
      val reqTprocess = sysRuntime.exec("java -jar reqT.jar")
      val (reqTis, reqTos) = (reqTprocess.getInputStream, reqTprocess.getOutputStream)
      var buf = new Array[Byte](1024)
      reqTos.write("\n\n".getBytes)
      reqTos.flush()
      Thread.sleep(100)
      var nbrOfReadBytes = reqTis.read(buf, 0, 1024)
      var response = buf.take(nbrOfReadBytes).map(_.toChar).mkString
      println(s"printed $response")
      out ! response
  //val templateHandler = new TemplateHandler

  def trim(text: String): String = text.drop(text.indexOf("reqT>"))

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def receive = {

    case message: String =>
//      val inputStream = new ByteArrayInputStream((message + "\n").getBytes("UTF-8"))
//      val processOutput = reqT.#<(inputStream).!!
//      out ! trim(processOutput)

//      val inputStream = new ByteArrayInputStream((message + "\n").getBytes("UTF-8"))
//      val processOutput = reqT.#<(inputStream)
//      out ! trim(processOutput.toString)

      //val inputStream = new ByteArrayInputStream((message + "\n").getBytes("UTF-8"))
      //out ! trim(reqT.run().toString)
      println(s"user wrote $message")
      reqTos.write((message + "\n").getBytes("UTF-8"))
      reqTos.flush()

      println(buf.length)
      Thread.sleep(100)
      var nbrOfReadBytes = reqTis.read(buf, 0, 1024)
      var response = buf.take(nbrOfReadBytes).map(_.toChar).mkString
      println(s"printed $response")
      out ! response
      nbrOfReadBytes = reqTis.read(buf, 0, 1024)
      response = buf.take(nbrOfReadBytes).map(_.toChar).mkString
      println(s"printed $response")
      out ! response

    //    case message: String =>
//      message match {
//        case r"Template[1-9][0-9]*" =>
//          out ! templateHandler.getTemplate(message)
//      }
  }
}
