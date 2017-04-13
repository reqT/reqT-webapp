package controllers

import javax.inject.Inject

import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.libs.streams.ActorFlow
import play.api.mvc._




class Application @Inject() (implicit system: ActorSystem, materializer: Materializer) extends Controller{

  def index = Action {
    Ok(views.html.index("hej"))
  }

  def socket = WebSocket.accept[String, String] { request =>
    ActorFlow.actorRef(out => MyWebSocketActor.props(out))
  }
}
