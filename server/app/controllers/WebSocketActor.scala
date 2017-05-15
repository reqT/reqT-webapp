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
    "Model(Goal(\"accuracy\") has Spec(\"On \"), Feature(\"quotation\") has Spec(\"Producata\"), Function(\"experienceData\") has Spec(\"Prod data\"), Design(\"screenX\") has Spec(\"Systin Fig\"))"
  )


  val sysRuntime = Runtime.getRuntime
  val reqTprocess = sysRuntime.exec("java -jar reqT.jar")
  val (reqTis, reqTos) = (reqTprocess.getInputStream, reqTprocess.getOutputStream)
  val buf = new Array[Byte](1024)

  def trim(text: String): String = text.drop(text.indexOf("reqT>"))
  def trimResponse(text: String): String = text.drop(text.indexOf("res"))

  def findEndOfModel(model: List[Char]): Int = {
    def go(cs: List[Char], level: Int): Int = cs match {
      case ')' :: _ if level==1 => 1
      case ')' :: xs => go(xs, level-1) + 1
      case '(' :: xs => go(xs, level+1) + 1
      case _  :: xs => go(xs, level) + 1
    }
    go(model, 0)
  }

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  val readThread = new Thread(new Runnable {
    override def run() = {
      while(reqTprocess.isAlive) {

        if(reqTis.available() > 0){
          val nbrOfReadBytes = reqTis.read(buf, 0, 1024)
          val response = buf.take(nbrOfReadBytes).map(_.toChar).mkString
          if (response.contains("= Model(")){
            val start = response.indexOf("= Model(")+2
            val end = findEndOfModel(response.toList)

            val result = parser.parseAll(parser.Model, response.slice(start,end))

            out ! write[Model](result.get)
          } else if(response.contains(":") && !response.contains("Welcome to")){
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
