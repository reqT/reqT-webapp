//package example
//
//import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
//import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent, ReactKeyboardEventI}
//import org.scalajs.dom.ext.KeyCode
//import japgolly.scalajs.react._
//import org.scalajs.dom
//import org.scalajs.dom.document
//import org.scalajs.dom.raw._
//import shared.Model
//import upickle.default.read
//
//import scala.util.{Failure, Success}
//
//
///**
//  * Created by phiped on 5/25/17.
//  */
//object Terminal {
//
//    def modalStyle = Seq(
//      ^.width := "400px",
//      ^.padding := "5px",
//      ^.position := "absolute",
//      ^.border := "1px solid #CCC",
//      ^.borderRadius := "5px",
//      ^.top := "40%",
//      ^.left := "50%",
//      ^.transform := "translate(-50%,-50%)",
//      ^.zIndex := "9999",
//      ^.background := "#FFF" ,
//      ^.paddingBottom := "15px",
//      ^.paddingRight := "15px" ,
//      ^.paddingTop := "15px",
//      ^.paddingLeft := "15px",
//      ^.boxShadow := "rgba(0, 0, 0, 0.2) 5px 6px 12px 0px"
//    )
//
//    def backdropStyle = Seq(
//      ^.position := "absolute",
//      ^.width := "100%",
//      ^.height := "100%",
//      ^.top := "0px",
//      ^.left := "0px",
//      ^.zIndex := "9998",
//      ^.background := "#CCC",
//      ^.opacity := "0.5"
//    )
//
//    def selectStyle = Seq(
//      ^.className := "form-control pull-right",
//      ^.width := "155px",
//      ^.height :=  "100%",
//      ^.color := "BLACK",
//      ^.background := "white",
//      ^.textAlign.center,
//      ^.textAlignLast.center
//    )
//
//    def buttonDivStyle = Seq(
//      ^.width := "95%",
//      ^.padding := "20px",
//      ^.display.flex,
//      ^.justifyContent.spaceBetween
//    )
//
//  case class State(websocket: Option[WebSocket], logLines: Vector[String], message: String){
//    def log(line: String): State = copy(logLines = logLines :+ line)
//
//  }
//
//    case class Props()
//
//    class Backend($: BackendScope[Props, State]) {
//      def render(P: Props, S: State) =
//        <.pre(
//          ^.height := "45%",
//          ^.overflow.hidden,
//          <.div(
//            <.input(
//              ^.className := "form-control",
//              ^.marginBottom := "5px"
//              //                ,
//              //                ^.onChange ==> onChange,
//              //                ^.value := S.message,
//              //                ^.onKeyDown ==>? handleKeyDown
//            ),
//            <.button(
//              ^.className := "btn btn-default",
//              //                ^.disabled := send.isEmpty,
//              //                ^.onClick -->? send,
//              "Send"),
//            <.button(
//              //                ^.disabled := sendVerify.isEmpty,
//              ^.className := "btn btn-default",
//              "Verify Model"
//              //                ,
//              //                ^.onClick -->? sendVerify
//            )
//          )
//          //            ,
//          //            log(S.logLines)// Display log
//        )
//    }
//
//
//  val log = ReactComponentB[Vector[String]]("log")
//    .render($ =>
//
//      <.pre(
//        ^.className := "form-control",
//        ^.id := "reqTLog",
//        ^.width := "auto",
//        ^.height := "80%",
//        ^.marginTop := "5px",
//        ^.overflowY.auto,
//        ^.overflowX.hidden,
//        ^.whiteSpace.`pre-line`,
//        $.props.map(<.p(_)))
//    )
//    .componentDidUpdate(_ => updateScroll)
//    .componentDidMount(_ => Callback({
//      var reqtLog = document.getElementById("reqTLog").asInstanceOf[dom.html.Pre]
//      reqtLog.setAttribute("style", "user-select:text;" + reqtLog.style.cssText)
//    }))
//    .build
//
//  def updateScroll: Callback = {
//    Callback({
//      var reqtLog = document.getElementById("reqTLog").asInstanceOf[dom.html.Pre]
//      reqtLog.scrollTop = reqtLog.scrollHeight
//    })
//  }
//
//  def start: Callback = {
//
//    // This will establish the connection and return the WebSocket
//    def connect = CallbackTo[WebSocket] {
//
//      // Get direct access so WebSockets API can modify state directly.
//      val direct = $.accessDirect
//
//      def onopen(event: Event): Unit = {
//        direct.modState(_.log("Connected."))
//      }
//
//
//      def onmessage(event: MessageEvent): Unit = {
//        if(event.data.toString.startsWith("{")){
//          val tree = read[Model](event.data.toString).tree
//          receiveModel(direct.state, tree)
//        } else {
//          direct.modState(_.log(s"${event.data.toString}"))
//        }
//        direct.modState(_.copy(waitingForModel = false))
//      }
//
//      def onerror(event: ErrorEvent): Unit = {
//        direct.modState(_.log(s"Error: ${event.message}"))
//      }
//
//      def onclose(event: CloseEvent): Unit = {
//        direct.modState(_.copy(websocket = None).log(s"Closed: ${event.reason}"))
//      }
//
//      // Create WebSocket and setup listeners
//      val websocket = new WebSocket(url)
//      websocket.onopen = onopen _
//      websocket.onclose = onclose _
//      websocket.onmessage = onmessage _
//      websocket.onerror = onerror _
//      websocket
//    }
//
//    // Here use attemptTry to catch any exceptions in connect.
//    connect.attemptTry.flatMap {
//      case Success(websocket) => $.modState(_.log("Connecting...").copy(websocket = Some(websocket)))
//      case Failure(error) => $.modState(_.log(error.toString))
//    }
//  }
//
//  def end: Callback = {
//    def closeWebSocket = $.state.map(_.websocket.foreach(_.close()))
//
//    def clearWebSocket = $.modState(_.copy(websocket = None))
//
//    closeWebSocket >> clearWebSocket
//  }
//
//
//
////    def apply() = component.set()(Props())
//
//}
