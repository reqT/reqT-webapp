package controllers

import akka.actor._
import shared._

import upickle.default._



object WebSocketActor {
  def props(out: ActorRef) = Props(new WebSocketActor(out))
}



class WebSocketActor(out: ActorRef) extends Actor {
  //val templateHandler = new TemplateHandler

  val parser = new ExprParser

  val result = parser.parseAll(parser.Model,
    "Model(" +
      "Req(\"R1\"), " +
      "Comment(\"R2\"), " +
      "Stakeholder(\"BOSS\"), " +
      "Req(\"R3\") helps " +
        "Model(Req(\"R3.1\") has " +
          "Model(Prio(1))), " +
      "Req(\"R4\") has " +
        "Model(Comment(\" hej\"), Feature(\" YOYO\")))"
  )


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
          if(response.contains(":") && !response.contains("Welcome to")){
            out ! ("Answer: \n" + response.replace("reqT>", "").drop(response.indexOf("res")))
          }
        } else {
          Thread.sleep(500)
        }
      }
    }
  }).start()

  testUpickle
  def testUpickle = {
    out ! write[Model](result.get)
  }

  def receive = {
    case message: String =>
      reqTos.write((trim(message) + "\n").getBytes("UTF-8"))
      reqTos.flush()



//      message match {
//        case r"Template[1-9][0-9]*" =>
//          out ! templateHandler.getTemplate(message)
//      }
  }
}
