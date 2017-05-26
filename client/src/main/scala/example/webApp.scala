package example

import diode.Action
import org.scalajs.dom._
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSImport}
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react._
import scala.util.{Failure, Success}
import diode.react.ModelProxy
import example.Modal.ModalType
import org.scalajs.dom.ext.{Ajax, KeyCode}
import org.scalajs.dom.raw._
import scalacss.ScalaCssReact._
import scalacss.Defaults._
import upickle.default._
import scala.collection.immutable.Queue
import shared._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.jquery.JQueryStatic

@js.native
@JSImport("jquery", JSImport.Namespace)
object jquery extends JQueryStatic

@JSExport
object webApp extends js.JSApp {
  //Måste ändras till hostname
  val url = "ws://127.0.0.1:9000/socket"

  /**
    * Example of how to fetch data from client. Problem is that in the case of entities/attributes, these are needed when the client application is started.
    * No blocking on UI does not allow for sync read from server.
    */
//    val statusValueAttrList = List()
//
//  def getStatusValueAttributes: Future[List[String]] = {
//    Ajax.get("/statusvalueattributes").map { r =>
//      println(read[List[String]](r.responseText))
//      read[List[String]](r.responseText)
//    }
//  }


//  case class modelProps(modalType: ModalType, treeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String], newElemToAdd: Option[Elem])

  case class Props(proxy: ModelProxy[Tree])

  case class CachedModel(name: String, model: Tree, selected: Boolean, uUID: UUID)

  case class OpenModals(isModalOpen: Boolean = false, isNewModelModalOpen: Boolean = false, isDollarModalOpen: Boolean = false,
                        isReleaseModalOpen: Boolean = false, isOrdinalModalOpen: Boolean = false)

  case class State(websocket: Option[WebSocket], logLines: Vector[String], message: String, modalType: ModalType, treeItem: TreeItem,
                   dispatch: (Action => Callback) = null, path: Seq[String] = Seq(), elemToModal: Option[Elem] = None,
                   cachedModels: Queue[CachedModel] = Queue(CachedModel("untitled", Tree(Seq()), selected = true, uUID = UUID.random())),
                   openModals: OpenModals = OpenModals(), saveModelType : String = "rec",
                   latestRecTree: Tree = Tree(Seq()), isMethodStarted: Boolean = false,
                   waitingForModel: Boolean = false, scrollPosition: Double = 0, newModel: Tree = emptyTree) {

    def log(line: String): State = copy(logLines = logLines :+ line)

    def saveTree(tree: Tree): State = copy(latestRecTree = tree)

  }

  val emptyTree = Tree(Seq())

  def elemToTreeItem(elems: Seq[Elem]): TreeItem = {
    TreeItem("Model", UUID.model(), elems.map(elem => convert(elem)), None)
  }

  def convert(elem: Elem): TreeItem = elem match {
    case relation: Relation => TreeItem(relation.entity, relation.entity.uuid, relation.submodel.children.map(convert), Some(relation.link))
    case node: shared.Node => TreeItem(node, node.uuid, Seq(), None)
  }


  import org.scalajs.dom
  import scala.scalajs.js.timers._


  class Backend($: BackendScope[Props, State]) {

    def closeModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isModalOpen = false)))
    def closeDollarModal(e: ReactEvent): Callback =  $.modState(S => S.copy(openModals = S.openModals.copy(isDollarModalOpen = false)))
    def closeReleaseModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isReleaseModalOpen = false)))
    def closeOrdinalModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isOrdinalModalOpen = false)))
    def closeNewModelModal(): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isNewModelModalOpen = false)))


    def openModalWithContent(modalType: ModalType, treeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String], newElemToAdd: Option[Elem] ): Callback
    = $.modState(_.copy(modalType = modalType, treeItem = treeItem, openModals = OpenModals(isModalOpen = true), dispatch = newDispatch, path = newPath, elemToModal = newElemToAdd))

    def openDollarModal = $.modState(_.copy(openModals = OpenModals(isDollarModalOpen = true)))
    def openOrdinalModal = $.modState(_.copy(openModals = OpenModals(isOrdinalModalOpen = true)))
    def openReleaseModal = $.modState(_.copy(openModals = OpenModals(isReleaseModalOpen = true)))
    def openNewModelModal(newSaveModelType: String, newModel: Tree) = $.modState(_.copy(openModals = OpenModals(isNewModelModalOpen = true),
      saveModelType = newSaveModelType, newModel = newModel))

    val treeView = ReactComponentB[(ModelProxy[Tree], (ModalType, TreeItem, (Action => Callback), Seq[String], Option[Elem]) => Callback)]("treeView")
      .render(P => <.pre(
//        ^.onScroll --> getScroll,
        Styles.treeView,
        ^.border := "1px solid #ccc",
        ^.id := "treeView",
        <.div(
          ReactTreeView(
            root = elemToTreeItem(P.props._1.value.children),
            openByDefault = true,
            modelProxy = P.props._1,
            showSearchBox = true,
            setModalContent = P.props._2
          ),
          <.strong(
            ^.id := "treeviewcontent"
          )
        )
      ))
      .build

    def setScroll(position : Double): Callback = {
      var temp = document.getElementById("treeView").asInstanceOf[dom.html.Pre]
      Callback(temp.scrollTop = temp.scrollHeight)
    }
    def getScroll: Callback ={
      $.modState(_.copy(scrollPosition = document.getElementById("treeView").scrollTop))
    }


    def saveModel(name: String, model: Tree, P: Props): Callback =
      $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, model, selected = false, UUID.random())))


    def render(P: Props, S: State) = {
      val sc = AppCircuit.connect(_.tree)

      val send: Option[Callback] ={
        for (websocket <- S.websocket if S.message.nonEmpty)
          yield sendMessage(websocket, S.message)
      }

      val sendVerify: Option[Callback] =
        for (websocket <- S.websocket if P.proxy.value.toString.nonEmpty)
          yield sendMessage(websocket, P.proxy.value.makeString.replaceAll("\n", ""))


//      val sendGetTemplate(templateNbr: Int): Option[Callback] =
//        for (websocket <- state.websocket if state.message.nonEmpty)
//          yield sendMessage(websocket, "Template" + templateNbr)

      def setMethodStarted: Callback = $.modState(_.copy(isMethodStarted = true))

      def handleKeyDown(event: ReactKeyboardEventI): Option[Callback] = {
        if (event.nativeEvent.keyCode == KeyCode.Enter)
          send
        else
          None
      }

      <.div(
        Modal(S.openModals.isModalOpen, closeModal, S.modalType, S.treeItem, S.dispatch, S.path, S.elemToModal),
        NewModelModal(isOpen = S.openModals.isNewModelModalOpen, onClose = closeNewModelModal, saveModel = saveModel(_, _, P),
          S.newModel, S.saveModelType),
        ^.className := "container",
        ^.width := "100%",
        ^.height := "100%",
        ^.overflow := "hidden",
        ^.paddingRight := "5px",
        ^.paddingLeft := "5px",
        <.div(
          ^.className := "header",
          Header(P.proxy, openNewModelModal)
        ),
        <.div(
          ^.className := "col-1",
          ^.float := "left",
          ^.width := "29%",
          ^.height := "100%",
          ^.paddingRight := "9px",
          ElementList(),
          <.pre(
            ^.height := "45%",
            ^.overflow.hidden,
            <.div(
              <.input(
                ^.className := "form-control",
                ^.marginBottom := "5px",
                ^.onChange ==> onChange,
                ^.value := S.message,
                ^.onKeyDown ==>? handleKeyDown
              ),
              <.button(
                ^.className := "btn btn-default",
              ^.disabled := send.isEmpty,
              ^.onClick -->? send,
              "Send"),
              <.button(
                ^.disabled := sendVerify.isEmpty,
                ^.className := "btn btn-default",
                "Verify Model",
                ^.onClick -->? sendVerify
              )
            ),
            log(S.logLines)// Display log
          )
        ),
        <.div(
          ^.className := "col-2",
          ^.width := "71%",
          ^.height := "100%",
          ^.float.left,
          cachedModels((P,S)),
//          CachedModels(P.proxy),
          sc(proxy => treeView((proxy, openModalWithContent)))
        )
      )



    }


    def setTemplate(child: Seq[Elem]): Callback = $.modState(_.copy(latestRecTree = Tree(child)))


    val cachedModels = ReactComponentB[(Props, State)]("cachedModelsComponent")
      .render($ => <.pre(
        ^.padding := "5px",
        ^.paddingRight := "5px",
        ^.height := "5%",
        ^.overflow := "hidden",
        ^.position.relative,
        <.button(
          ^.className := "glyphicon glyphicon-plus",
          ^.color := "green",
          ^.position.absolute,
          ^.top := "0%",
          ^.width := "5%",
          ^.height := "105%",
          ^.marginLeft := "-6px",
          ^.marginTop := "-2px",
          Styles.navBarButton,
          ^.outline := "none",
          ^.onClick --> openNewModelModal("save", $.props._1.proxy.value)
        ),
        <.div(
          ^.overflowX.auto,
          ^.left := "5%",
          ^.height := "91%",
          ^.overflowX.auto,
          ^.overflowY.hidden,
          ^.width := "95%",
          ^.position.absolute,
          <.div(
            ^.whiteSpace := "nowrap",
            ^.position.absolute,
            ^.className := "clickable-row",
              <.ul(
                ^.display.flex,
                ^.height := "0px",
                ^.className := "nav nav-pills",
                ^.listStyleType.none,
                $.props._2.cachedModels.reverse.map(s => listModels((s, $.props._1, $.props._2)))
              )
          )
        )
      )
      ).build

    val listModels = ReactComponentB[(CachedModel, Props, State)]("listElem")
      .render($ => <.li(
        ^.className := "navpill",
        ^.display := "inline",
        ^.whiteSpace := "nowrap",
        ^.position.relative,
        ^.marginLeft := "5px",
        ^.marginRight := "5px",
        ^.padding := "5px",
        ^.float.left,
        ^.overflow := "hidden",
        ^.borderRadius := "5px",
        ^.height := "30px",
        ^.top := "0px",
        ^.width := "200px",
        ^.background := "#CFEADD",
        ^.opacity := {if($.props._1.selected) "1" else "0.5"},

        <.span(
          ^.className := "col",
          ^.position.absolute,
          ^.width := "80%",
          ^.height := "30px",
          ^.paddingTop := "2px",
          ^.textAlign := "center",
          $.props._1.name
        ),
        ^.onClick --> setActiveModel($.props._1, $.props._2, $.props._3),

        <.button(
          ^.className := "col",
          ^.position.absolute,
          ^.width := "20%",
          ^.height := "30px",
          ^.left := "80%",
          ^.top := "0%",
          ^.paddingTop := "5px",
          Styles.removeButtonSimple,
          ^.outline := "none",
          ^.onClick ==> removeCachedModel($.props._3, $.props._1)
        )
      )).build


    def setActiveModel(cachedModel: CachedModel,P: Props, S: State): Callback = {
      updateActiveModel(cachedModel, P,S) >> P.proxy.dispatchCB(SetModel(cachedModel.model.children))
    }

    def activeModel(activeModel: CachedModel, cachedModels: Queue[CachedModel]): Callback = $.modState(_.copy(cachedModels = cachedModels.map(
      model => if(model.uUID.equals(activeModel.uUID)) model.copy(selected = true) else model.copy(selected = false))))

    def updateActiveModel(cachedModel: CachedModel, P: Props, S: State): Callback = {
      val newModels: Queue[CachedModel] = S.cachedModels.map(model =>
        if (model.selected)
          model.copy(model = P.proxy.value, selected = model.uUID.equals(cachedModel.uUID))
        else
          model.copy(selected = model.uUID.equals(cachedModel.uUID))
      )

      $.modState(_.copy(cachedModels = newModels))
    }

    def removeCachedModel(state: State, modelToRemove: CachedModel)(e: ReactEventI): Callback = {
      e.stopPropagation()
      val index = state.cachedModels.indexWhere(_.equals(modelToRemove))
      val beginning = state.cachedModels.take(index)
      val end = state.cachedModels.drop(index+1)
      $.modState(_.copy(cachedModels = beginning ++ end))
    }

    val log = ReactComponentB[Vector[String]]("log")
      .render($ =>
        <.pre(
          ^.className := "form-control",
          ^.id := "reqTLog",
          ^.width := "auto",
          ^.height := "80%",
          ^.marginTop := "5px",
          ^.overflowY.auto,
          ^.overflowX.hidden,
          ^.whiteSpace.`pre-line`,
          $.props.map(<.p(_)))
      )
      .componentDidUpdate(_ => updateScroll)
        .componentDidMount(_ => Callback({
          var reqtLog = document.getElementById("reqTLog").asInstanceOf[dom.html.Pre]
          reqtLog.setAttribute("style", "user-select:text;" + reqtLog.style.cssText)
        }))
      .build

    def updateScroll: Callback = {
      Callback({
        var reqtLog = document.getElementById("reqTLog").asInstanceOf[dom.html.Pre]
        reqtLog.scrollTop = reqtLog.scrollHeight
      })
    }

    def onChange(event: ReactEventI): Callback = {
      val newMessage = event.target.value
      $.modState(_.copy(message = newMessage))
    }


    def sendMessage(websocket: WebSocket, msg: String): Callback = {
      def send(msg : String) = Callback(websocket.send(msg))
      def updateState = $.modState(s => s.log(s"Sent: \n$msg").copy(message = ""))
      def setStateToCatchModel = $.modState(_.copy(waitingForModel = true))

      if(msg.startsWith("get "))
        setStateToCatchModel >> send(msg.replaceFirst("get ", "")) >> updateState
      else
        send(msg) >> updateState
    }

    def sendMessages(websocket: WebSocket, msg: Seq[String]): Seq[Callback] = msg.map(sendMessage(websocket,_))

    def receiveModel(S: State, tree: Tree) = {
      $.accessDirect.modState(_.saveTree(tree))

      if (S.isMethodStarted || S.waitingForModel){
//        openNewModelModal("rec", ).runNow()
        $.accessDirect.modState(_.copy(isMethodStarted = false))
      }
    }


    def start: Callback = {

      // This will establish the connection and return the WebSocket
      def connect = CallbackTo[WebSocket] {

        // Get direct access so WebSockets API can modify state directly.
        val direct = $.accessDirect

        def onopen(event: Event): Unit = {
          direct.modState(_.log("Connected."))
        }


        def onmessage(event: MessageEvent): Unit = {
          if(event.data.toString.startsWith("{")){
            val tree = read[Model](event.data.toString).tree
            receiveModel(direct.state, tree)
          } else {
            direct.modState(_.log(s"${event.data.toString}"))
          }
          direct.modState(_.copy(waitingForModel = false))
        }

        def onerror(event: ErrorEvent): Unit = {
          direct.modState(_.log(s"Error: ${event.message}"))
        }

        def onclose(event: CloseEvent): Unit = {
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

  val pageContent = ReactComponentB[Props]("Content")
    .initialState(State(None, Vector.empty, message = "", modalType = Modal.EMPTY_MODAL, treeItem = null))
    .renderBackend[Backend]
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.end)
    .componentDidUpdate( _ => Callback(println("hej")))
    .build

  val dc = AppCircuit.connect(_.tree)

  def main(): Unit = {
    Styles.addToDocument()
    ReactDOM.render(dc(proxy => pageContent(Props(proxy))), document.getElementById("content"))
  }
}
