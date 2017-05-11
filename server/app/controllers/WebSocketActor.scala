package controllers
import java.io.ByteArrayInputStream

import akka.actor._


import scala.sys.process._


object WebSocketActor {
  def props(out: ActorRef) = Props(new WebSocketActor(out))
}



class WebSocketActor(out: ActorRef) extends Actor {
  //val templateHandler = new TemplateHandler


  val sysRuntime = Runtime.getRuntime
  val reqTprocess = sysRuntime.exec("java -jar reqT.jar")
  val (reqTis, reqTos) = (reqTprocess.getInputStream, reqTprocess.getOutputStream)
  val buf = new Array[Byte](1024)

  def trim(text: String): String = text.drop(text.indexOf("reqT>"))
  def trimResponse(text: String): String = text.drop(text.indexOf("res"))

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  val readThread = new Thread(new Runnable {
    override def run() = {
      while(reqTprocess.isAlive) {

        if(reqTis.available() > 0){
          var nbrOfReadBytes = reqTis.read(buf, 0, 1024)
          var response = buf.take(nbrOfReadBytes).map(_.toChar).mkString
          println(response.length)
          println(response)
          if(response.contains(":") && !response.contains("Welcome to")){
            out ! ("Answer: \n" + response.replace("reqT>", "").drop(response.indexOf("res")))
          }
        } else {
          Thread.sleep(500)
        }
      }
    }
  }).start()

  def receive = {
    case message: String =>
      println(s"user wrote $message")
      reqTos.write((trim(message) + "\n").getBytes("UTF-8"))
      reqTos.flush()

//      message match {
//        case r"Template[1-9][0-9]*" =>
//          out ! templateHandler.getTemplate(message)
//      }
  }
}
