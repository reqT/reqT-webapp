package controllers

import javax.inject.Inject

import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.libs.streams.ActorFlow
import play.api.mvc._
import shared.{ Class => Clazz, _}
import upickle.default._


class Application @Inject() (implicit system: ActorSystem, materializer: Materializer) extends Controller{
  val templateHandler = new TemplateHandler
  val elemListHandler = new ElementListHandler

  def index = Action {
    Ok(views.html.index("hej"))
  }

  def socket = WebSocket.accept[String, String] { request =>




    def instantiate[T](clazz: java.lang.Class[T])(args:AnyRef*): T = {
      val constructor = clazz.getConstructors()(0)
      return constructor.newInstance(args:_*).asInstanceOf[T]
    }


    def getClass(s: String) = Class.forName(s"shared.$s")

    val cls = getClass("Req")
    val c = instantiate(cls)(new java.lang.String("hej"), UUID.random())

    val cls1 = Class.forName("shared.Relation")
    val c1 = instantiate(cls1)(Req(), has, Tree(Seq(Req())))
//
//    val cls2 = Class.forName("shared.has")
//    val c2 = instantiate(cls2)
//    cls2.newInstance()

    println(c)

    ActorFlow.actorRef(out => WebSocketActor.props(out))
  }

  def getEntities = Action {
    Ok(write[List[String]](elemListHandler.getEntities))
  }

  def getIntAttributes = Action {
    Ok(write[List[String]](elemListHandler.getIntAttributes))
  }

  def getStringAttributes = Action {
    Ok(write[List[String]](elemListHandler.getStringAttributes))
  }

  def getStatusValueAttributes = Action {
    Ok(write[List[String]](elemListHandler.getStatusValueAttributes))
  }

//  def template(templateNbr: Int) = Action {
//    Ok(templateHandler.getTemplate(templateNbr).getOrElse(NotFound))
//  }


}
