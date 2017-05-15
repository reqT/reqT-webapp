package controllers

import akka.actor._
import shared._
import scala.util.parsing.combinator.RegexParsers
import upickle.default._



object WebSocketActor {
  def props(out: ActorRef) = Props(new WebSocketActor(out))
}



class WebSocketActor(out: ActorRef) extends Actor {
  //val templateHandler = new TemplateHandler


  class ExprParser extends RegexParsers {

    val int = "[0-9][0-9]*".r
    val string = "[a-zA-Z0-9~!@#$^%&*|;:'{}\\[\\] /<,>.+?`-][a-zA-Z0-9~!@#$^%&*|;:'{}\\[\\] /<,>.+?`-]*".r
    val relType = "[a-zA-Z][a-zA-Z]*".r

    def Model: Parser[shared.Model] = "Model(" ~ opt(rep(Elem ~ ",")) ~ Elem ~")"^^{
      case   _ ~ Some(list) ~ elem ~ _=> {
        new Model(Tree(list.map(_._1) ++ Seq(elem.asInstanceOf[shared.Elem])))
      }
    }

    def Elem: Parser[shared.Elem] = Relation | Node

    def Relation: Parser[shared.Relation] = Entity ~ RelationType ~ Model^^{
      case ent ~ reltype ~ model => new Relation(ent,reltype, model.tree)
    }

    def RelationType: Parser[shared.RelationType] = relType ^^{new RelationType(_)}

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

    def Entity: Parser[shared.Entity] = string ~ "(" ~ "\"" ~ opt(string) ~ "\"" ~ ")" ^^{
      case tpe ~ _ ~  _ ~ Some(id) ~ _ ~ _ => new Entity(tpe,id)
      case tpe ~ _ ~  _ ~ None ~ _ ~ _ => new Entity(tpe)
    }
  }


  val parser = new ExprParser

//  val result = parser.parseAll(parser.Model,
//    "Model(Goal(\"accuracy\") has Model(Spec(\"Our pre-calculations shall hit within 5%\")), Feature(\"quotation\") has Model(Spec(\"Product shall support cost recording and quotation with experience data\")), Function(\"experienceData\") has Model(Spec(\"Product shall have recording and retrieval functions for experience data\")), Design(\"screenX\") has Model(Spec(\"System shall have screen pictures as shown in Fig. X\")))"
//  )


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

//  testUpickle
//  def testUpickle = {
//    out ! write[Model](result.get)
//  }

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
