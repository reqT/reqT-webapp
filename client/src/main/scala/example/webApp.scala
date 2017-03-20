package example

import diode.Action
import org.scalajs.dom._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scala.util.{Failure, Success}
import diode.react.ModelProxy
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.raw.{HTMLElement, HTMLStyleElement}

import scala.scalajs.js.JSON
import scalacss.ScalaCssReact._
import scalacss.Defaults._
import upickle.default._


@JSExport
object webApp extends js.JSApp {

  val url = "ws://127.0.0.1:9000/socket"
  //val entities = List("Req", "Stakeholder", "Label", "User", "Item", "Label", "Meta", "Section", "Term", "Actor", "App", "Component", "Domain", "Module", "Product", "Release", "Resource", "Risk", "Service", "System", "User")
  val elems = List(Item(""), Label(""), Meta(""), Section(""),Term(""), Actor(""), App(""), Component(""), Module(""), Product(""), Release(""), Resource(""),
    Risk(""), Service(""), Stakeholder(""), System(""), User(""), Class(""), Data(""), Input(""), Member(""),Output(""), Relationship(""), Design(""), Screen(""), MockUp(""), Function(""),
    Interface(""), Epic(""), Feature(""), Goal(""), Idea(""), Issue(""), Req(""), Ticket(""), WorkPackage(""), Breakpoint(""), Barrier(""), Quality(""), Target(""), Scenario(""), Task(""),
    Test(""), Story(""), UseCase(""), VariationPoint(""), Variant(""), Code(""), Comment(""), Deprecated(""), Example(""), Expectation(""), FileName(""), Gist(""), Image(""), Spec(""),
    Text(""), Title(""), Why(""), Benefit(0), Capacity(0), Cost(0), Damage(0), Frequency(0), Min(0), Max(0), Order(0), Prio(0), Probability(0), Profit(0), Value(0))

  val headerButtons = List(("Export", NoAction), ("Import", NoAction), ("Release Planning", NoAction), ("Templates", SetTemplate), ("Help", NoAction))

  case class Props(proxy: ModelProxy[Tree])

  case class State(websocket: Option[WebSocket], logLines: Vector[String], message: String, elems: List[Elem], content: String) {
    // Create a new state with a line added to the log
    def log(line: String): State =
      copy(logLines = logLines :+ line)
  }


  def dragStart(elem: Elem)(event: ReactDragEvent): Callback = {
    event.dataTransfer.effectAllowed = "move"
    event.dataTransfer.setData("existing", "false")
    elem match {
      case entity: Entity =>
        event.dataTransfer.setData("type", "entity")
        Callback(event.dataTransfer.setData("elem", write[Entity](entity)))
      case attribute: StringAttribute =>
        event.dataTransfer.setData("type", "stringAttr")
        Callback(event.dataTransfer.setData("elem", write[StringAttribute](attribute)))
      case attribute: IntAttribute =>
        event.dataTransfer.setData("type", "intAttr")
        Callback(event.dataTransfer.setData("elem", write[IntAttribute](attribute)))
      case _ =>
        event.dataTransfer.setData("type", "invalid")
        Callback(println("Dragged element is not valid"))
    }
  }


  def elemToTreeItem(elems: Seq[Elem]): TreeItem = {
    TreeItem("Model()", elems.map(elem => convert(elem)), None)

  }

  def convert(elem: Elem): TreeItem = elem match {
    case relation: Relation => TreeItem(relation.entity, relation.submodel.children.map(convert), Some(relation.link))
    case node: Node => TreeItem(node, Seq(), None)
  }

  val searchView = ReactComponentB[Unit]("searchView")
    .render(_ =>
      <.pre(
        Styles.searchView,
        ^.paddingLeft := "10px",
        ^.border := "1px solid #ccc",
        ^.overflow := "auto",
        ^.id := "searchView"

      )
    )
    .build

  val listElem = ReactComponentB[Elem]("listElem")
    .render($ => <.ul(
      $.props.toString.takeWhile(_!='('),
      Styles.listElem,
      ^.id := $.props.toString,
      ^.classID := $.props.toString,
      ^.background := (if ($.props.isEntity) "#eee" else "#ddd"),
      ^.padding := 5.px,
      ^.draggable := "true",
      ^.onDragStart ==> dragStart($.props)
    ))
    .build


  val entityListView = ReactComponentB[List[Elem]]("entityList")
    .render(elems => <.pre(
      ^.id := "dragList",
      ^.height := "80%",
      ^.border := "1px solid #ccc",
      ^.overflow := "auto",
      elems.props.sortWith((a,b) => a.toString < b.toString).map(listElem(_))
    ))
    .build


  val treeView = ReactComponentB[ModelProxy[Tree]]("treeView")
    .render(proxy => <.pre(
      Styles.treeView,
      ^.border := "1px solid #ccc",
      ^.id := "treeView",
      <.div(
        ReactTreeView(
          root = elemToTreeItem(proxy.props.value.children),
          openByDefault = true,
          modelProxy = proxy.props,
          showSearchBox = true
          //onItemSelect = onItemSelect _
        ),
        <.strong(
          ^.id := "treeviewcontent"
        )
      )
    )
    ).build

  val buttonComponent = ReactComponentB[(String, Action)]("buttonComponent")
    .render($ =>
      <.button(
        ^.padding := "10px",
        $.props._1,
        //^.onClick ==> HandleButtonPress($.props._2),
        Styles.navBarButton
      )
    ).build




  val navigationBar = ReactComponentB[Seq[(String, Action)]]("navigationBar")
    .render($ => <.nav(
      ^.paddingLeft := "15px",
      ^.paddingRight := "15px",
      Styles.navBar,
      $.props.map(buttonComponent(_))
    )
    ).build


  class Backend($: BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      val dispatch: Action => Callback = props.proxy.dispatchCB
      val sc = AppCircuit.connect(_.tree)

      // Can only send if WebSocket is connected and user has entered text
      val send: Option[Callback] =
        for (websocket <- state.websocket if state.message.nonEmpty)
          yield sendMessage(websocket, state.message)


      /**
        * Previous functionality, which will be used at a later stage (Server integration)
        */

      <.div(
        ^.className := "container",
        ^.width := "100%",
        ^.height := "100%",
        //        ^.paddingLeft := "15px",
        <.div(
          ^.className := "col-1",
          ^.float := "left",
          ^.width := "20%",
          ^.height := "100%",
          entityComponent(state.elems),
          //          searchView(state.content)
          <.pre(
            ^.height := "49%",
            <.h4("Type a message to get it reversed:"),
            <.div(
              <.input(
                ^.onChange ==> onChange,
                ^.value := state.message),
              ^.onKeyDown ==>? handleNewTodoKeyDown(dispatch),
              <.button(
                ^.disabled := send.isEmpty, // Disable button if unable to send
                ^.onClick -->? send, // --> suffixed by ? because it's for Option[Callback]
                "Reverse")),
            <.h4("Connection log"),
            log(state.logLines), // Display log
            <.p("Write \'reqT\' to start reqT")
          )
        ),
        <.div(
          ^.height := "100%",
          sc(proxy => treeView(proxy))
        )
        //        <.button(
        //          ^.border := "1px solid",
        //          Styles.bootstrapButton,
        //          ^.onClick --> dispatch(Reset),
        //          "Reset"
        //        ),
        //        <.button(
        //          ^.border := "1px solid",
        //          Styles.bootstrapButton,
        //          ^.onClick --> dispatch(RemoveElem(Seq())),
        //          "Remove"
        //        )
      )
    }


    def handleNewTodoKeyDown(dispatch: Action => Callback)(event: ReactKeyboardEventI): Option[Callback] = {
      if (event.nativeEvent.keyCode == KeyCode.Enter)
        Some(dispatch(Test))
      else
        None
    }



    val entityComponent = ReactComponentB[List[Elem]]("entityComponent")
      .render(elemList =>
        <.pre(
          Styles.dragList,
          <.form(
            <.input.text(
              ^.placeholder := "Search..",
              ^.onChange ==> onTextChange
            )
//            ,
//            <.p(
//              <.input.checkbox(
//                ^.onClick ==> filterEntities
//              ),
//              "Entities",
//              <.input.checkbox(
//                ^.onClick ==> filterAttr
//              ),
//              "Attributes"
//            )
          ),
          entityListView(elemList.props)
        )
      )
      .build

    val log = ReactComponentB[Vector[String]]("log")
      .render($ =>
        <.pre(
          ^.width := 360.px,
          ^.height := 200.px,
          ^.border := "1px solid",
          ^.overflow := "auto",
          $.props.map(<.p(_)))
      )
      .build

    def onTextChange(event: ReactEventI) =
      event.extract(_.target.value) {
        case "entity" => $.modState(_.copy(elems = elems.filter(_.isEntity)))
        case "attribute" => $.modState(_.copy(elems = elems.filter(_.isAttribute)))
        case value =>
          if(value.toLowerCase.startsWith("entity:"))
            $.modState(_.copy(elems = elems.filter(_.isEntity).filter(_.toString.toLowerCase.contains(value.drop(7).trim.toLowerCase))))
          else if(value.toLowerCase.startsWith("attribute:"))
            $.modState(_.copy(elems = elems.filter(_.isAttribute).filter(_.toString.toLowerCase.contains(value.drop(11).trim.toLowerCase))))
          else
            $.modState(_.copy(elems = elems.filter(_.toString.toLowerCase.contains(value.toLowerCase))))
      }

//    def filterEntities(event: ReactEvent) = $.modState(_.copy(elems = elems.filter(_.isEntity)))
//
//    def filterAttr(event: ReactEvent) = $.modState(_.copy(elems = elems.filter(_.isAttribute)))


    def onChange(event: ReactEventI): Callback = {
      val newMessage = event.target.value
      $.modState(_.copy(message = newMessage))
    }

    def sendMessage(websocket: WebSocket, msg: String): Callback = {
      // Send a message to the WebSocket
      def send = Callback(websocket.send(msg))

      // Update the log, clear the text box
      def updateState = $.modState(s => s.log(s"Sent: ${s.message}").copy(message = ""))

      send >> updateState
    }

    def start: Callback = {

      // This will establish the connection and return the WebSocket
      def connect = CallbackTo[WebSocket] {

        // Get direct access so WebSockets API can modify state directly
        // (for access outside of a normal DOM/React callback).
        val direct = $.accessDirect

        // These are message-receiving events from the WebSocket "thread".

        def onopen(event: Event): Unit = {
          // Indicate the connection is open
          direct.modState(_.log("Connected."))
        }

        def onmessage(event: MessageEvent): Unit = {
          // Echo message received
          direct.modState(_.log(s"Reversed: ${event.data.toString}"))
        }

        def onerror(event: ErrorEvent): Unit = {
          // Display error message
          direct.modState(_.log(s"Error: ${event.message}"))
        }

        def onclose(event: CloseEvent): Unit = {
          // Close the connection
          direct.modState(_.copy(websocket = None).log(s"Closed: ${event.reason}"))
        }

        // Create WebSocket and setup listeners
        val websocket = new WebSocket(url)
        websocket.onopen = onopen _
        websocket.onclose = onclose _
        websocket.onmessage = onmessage _
        websocket.onerror = onerror _
        websocket
      }

      // Here use attemptTry to catch any exceptions in connect.
      connect.attemptTry.flatMap {
        case Success(websocket) => $.modState(_.log("Connecting...").copy(websocket = Some(websocket)))
        case Failure(error) => $.modState(_.log(error.toString))
      }
    }

    def end: Callback = {
      def closeWebSocket = $.state.map(_.websocket.foreach(_.close()))

      def clearWebSocket = $.modState(_.copy(websocket = None))

      closeWebSocket >> clearWebSocket
    }
  }

  val Content = ReactComponentB[Props]("Content")
    .initialState(State(None, Vector.empty, "", elems, ""))
    .renderBackend[Backend]
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.end)
    .build

  val dc = AppCircuit.connect(_.tree)

  def main(): Unit = {
    Styles.addToDocument()
    ReactDOM.render(navigationBar(headerButtons), document.getElementById("header"))
    ReactDOM.render(dc(proxy => Content(Props(proxy))), document.getElementById("content"))
  }
}
