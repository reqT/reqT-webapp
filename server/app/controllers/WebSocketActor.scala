package controllers
import java.io.ByteArrayInputStream

import akka.actor._
import shared._

import scala.util.parsing.combinator.RegexParsers
import scala.sys.process._


object WebSocketActor {
  def props(out: ActorRef) = Props(new WebSocketActor(out))
}



class WebSocketActor(out: ActorRef) extends Actor {
  //val templateHandler = new TemplateHandler


  class ExprParser extends RegexParsers {

    val int = "[0-9][0-9]*".r
    val string = "[a-zA-Z1-9.!?-][a-zA-Z1-9.!?-]+".r

    def Model: Parser[shared.Model] = "Model(" ~ opt(rep(Elem ~ ",")) ~ Elem ~")"^^{
      case   _ ~ Some(list) ~ elem ~ _=> {
        new Model(Tree(list.map(_._1) ++ Seq(elem.asInstanceOf[shared.Elem])))
      }
    }

    def Elem: Parser[shared.Elem] = Relation | Node

    def Relation: Parser[shared.Relation] = Entity ~ RelationType ~ Model^^{
      case ent ~ reltype ~ model => new Relation(ent,reltype, model.tree)
    }

    def RelationType: Parser[shared.RelationType] = string ^^{new RelationType(_)}

    def Node: Parser[shared.Node] = Attribute | Entity ^^{
      case node :Node => node
    }

    def Attribute: Parser[shared.Attribute] = IntAttribute //| StringAttribute

    def IntAttribute: Parser[shared.IntAttribute] = string ~ "(" ~ opt(int) ~ ")" ^^{
      case tpe ~ _ ~ Some(value) ~ _ => new IntAttribute(tpe,value.toInt)
    }

//    def StringAttribute: Parser[StringAttribute] = string ~ "(\"" ~ opt(string) ~ "\")" ^^{
//      case tpe ~ _ ~ Some(value) ~ _ => new StringAttribute(tpe,value)
//    }

    def Entity: Parser[shared.Entity] = string ~ "(\"" ~ opt(string) ~ "\")" ^^{
      case tpe ~ _ ~ Some(id) ~ _ => new Entity(tpe,id)
      case tpe ~ _ ~ None ~ _ => new Entity(tpe)
    }
  }


  val parser = new ExprParser

  val result = parser.parseAll(parser.Model,
    "Model(" +
      "Req(\"R1\"), " +
      "Req(\"R2\"), " +
      "Stakeholder(\"BOSS\"), " +
      "Req(\"R3\") has " +
        "Model(Req(\"R3.1\") has " +
          "Model(Prio(1))), " +
      "Req(\"R4\") has " +
        "Model(Comment(\" hej\")))"
  )

  println(result)
  


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
