package example

import diode.Action
import org.scalajs.dom._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react._
import scala.scalajs.js.dom._
import scala.util.{Failure, Success}
import diode.react.ModelProxy
import example.Modal.ModalType
import org.scalajs.dom.ext.{Ajax, KeyCode}
import scalacss.ScalaCssReact._
import scalacss.Defaults._
import upickle.default._
import scala.scalajs.js.URIUtils._
import scala.collection.immutable.Queue
import shared._
import scala.concurrent.ExecutionContext.Implicits.global

@JSExport
object webApp extends js.JSApp {

  //Måste ändras till hostname

  val url = "ws://127.0.0.1:9000/socket"

  val entities = List("Ent", "Meta", "Item", "Label", "Section", "Term", "Actor", "App", "Component", "Domain", "Module", "Product", "Release", "Resource", "Risk", "Service",
    "Stakeholder", "System", "User", "Class", "Data", "Input", "Member", "Output", "Relationship", "Design", "Screen", "MockUp", "Function", "Interface", "State", "Event",
    "Epic", "Feature", "Goal", "Idea", "Issue", "Req", "Ticket", "WorkPackage", "Breakpoint", "Barrier", "Quality", "Target", "Scenario", "Task", "Test", "Story", "UseCase",
    "VariationPoint", "Variant")
  val intAttribute = List("Benefit", "Capacity", "Cost", "Damage", "Frequency", "Min", "Max", "Order", "Prio", "Probability", "Profit", "Value")
  val stringAttribute = List("Comment", "Deprecated", "Example", "Expectation", "FileName", "Gist", "Image", "Spec", "Text", "Title", "Why")


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

  val attributes = intAttribute ++ stringAttribute
  val elems = entities ++ attributes

  val headerButtons = Seq("Export", "Import", "Templates", "Help")


  case class Props(proxy: ModelProxy[Tree])

  case class State(websocket: Option[WebSocket], logLines: Vector[String], message: String, elems: Seq[String], isModalOpen: Boolean, modalType: ModalType,
                   treeItem: TreeItem, dispatch: (Action => Callback) = null, path: Seq[String] = Seq(), elemToModal: Option[Elem] = None,
                   entityChecked : Boolean = false, attributeChecked: Boolean = false, cachedModels: Queue[Tree] = Queue(),
                   isDollarModalOpen: Boolean = false, isReleaseModal: Boolean = false, latestRecTree: Tree = null) {

    def log(line: String): State = copy(logLines = logLines :+ line)

    def saveTree(tree: Tree): State ={
      copy(latestRecTree = tree)
    }

  }



  def dragStart(elem: Elem)(event: ReactDragEvent): Callback = {
    event.dataTransfer.effectAllowed = "move"
    event.dataTransfer.setData("existing", "false")
    elem.setRandomUUID()
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
    TreeItem("Model", UUID.model(), elems.map(elem => convert(elem)), None)

  }

  def convert(elem: Elem): TreeItem = elem match {
    case relation: Relation => TreeItem(relation.entity, relation.entity.uuid, relation.submodel.children.map(convert), Some(relation.link))
    case node: shared.Node => TreeItem(node, node.uuid, Seq(), None)
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


  val listElem = ReactComponentB[String]("listElem")
    .render($ => <.ul(
      $.props.toString.takeWhile(_!='('),
      Styles.listElem,
      ^.boxShadow := "0px 6px 12px 0px rgba(0,0,0,0.2)",
      ^.id := $.props.toString,
      ^.classID := $.props.toString,
      ^.background := (if (entities.contains($.props)) "#CEDBE7" else "#CFEADD"),
      ^.padding := 5.px,
      ^.borderRadius := "5px",
      ^.draggable := "true",
      ^.onDragStart ==> (
        if (entities.contains($.props)) dragStart(Entity($.props))
      else if(intAttribute.contains($.props)) dragStart(IntAttribute($.props))
      else dragStart(StringAttribute($.props))
        )
    ))
    .build



  val entityListView = ReactComponentB[Seq[String]]("entityList")
    .render(elems => <.pre(
      ^.className := "form-control",
      ^.id := "dragList",
      ^.height := "86%",
      ^.marginTop := "5px",
      ^.overflow := "auto",
      elems.props.sorted.map(listElem(_))
    ))
    .build

  val treeView = ReactComponentB[(ModelProxy[Tree], (ModalType, TreeItem, (Action => Callback), Seq[String], Option[Elem]) => Callback)]("treeView")
    .render(P => <.pre(
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
    )
    ).build

  def fixInputModel(tree: Tree): Tree = {
    var t = tree
    if(tree.children.nonEmpty) {
      val newChildren = tree.children.map({
        case child: Entity => if(stringAttribute.contains(child.getType)){
          StringAttribute(child.getType, child.getID)
        }else{
          child
        }
        case child: Relation => if(child.submodel.children.nonEmpty) {
          Relation(child.entity, child.link , fixInputModel(child.submodel))
        }else {
          child
        }
        case child: IntAttribute => child
      })
      t = Tree(newChildren)
    }
    t
  }

  def parseModel(newModel: String, dispatch: Action => Callback): Unit = {
    Ajax.get("/getmodelfromstring/" + encodeURI(newModel.trim.replaceAll(" +", " "))).onComplete{
      case Success(r) =>
        println(read[Model](r.responseText))
        dispatch(SetModel(fixInputModel(read[Model](r.responseText).tree).children)).runNow()
      case Failure(e) =>
        println(e.toString)
    }
  }

  def importModel(dispatch: Action => Callback)(e: ReactEventI): Callback = {
    if(e.currentTarget.files.item(0).`type` == "text/plain") {
      println(e.currentTarget.files.item(0).`type`)
      var newModel = "newModel empty, shouldn't happen"
      val fileReader = new FileReader
      println(e.currentTarget.files.item(0))
      fileReader.readAsText(e.currentTarget.files.item(0), "UTF-8")

      fileReader.onload = (_: UIEvent) => {
        newModel = fileReader.result.asInstanceOf[String]
        println("Kom in i FileReader")
        parseModel(newModel.replace("\n", "").trim, dispatch)
      }
      Callback(e.currentTarget.value = "")
    } else {
      Callback(window.alert("Invalid file type, only .txt is supported"))
    }
 }

  val buttonComponent = ReactComponentB[(String, Props)]("buttonComponent")
    .render($ =>
      $.props._1 match {
        case "Templates" => TemplateSelect($.props._2.proxy.dispatchCB)
        case "Import" => <.label(
          Styles.navBarButton,
          "Import",
          <.input(
            ^.`type`:="file",
            ^.display.none,
            ^.accept := "text/plain, .txt",
            ^.onChange ==> importModel($.props._2.proxy.dispatchCB)
          )
        )
        case _ => <.button(
          $.props._1,
          Styles.navBarButton,
          ^.onClick --> Callback(Parser)
        )
      }).build


  val navigationBar = ReactComponentB[(Seq[String], Props)]("navigationBar")
    .render($ => <.nav(
      ^.paddingLeft := "15px",
      ^.paddingRight := "15px",
      Styles.navBar,
      $.props._1.map(x => buttonComponent((x, $.props._2)))
    )
    ).build


  class Backend($: BackendScope[Props, State]) {

    def closeModal(e: ReactEvent): Callback = $.modState(_.copy(isModalOpen = false))
    def closeDollarModal(e: ReactEvent): Callback = $.modState(_.copy(isDollarModalOpen = false))
    def closeReleaseModal(e: ReactEvent): Callback = $.modState(_.copy(isReleaseModal = false))

    def openModalWithContent(modalType: ModalType, treeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String], newElemToAdd: Option[Elem] ): Callback
    = $.modState(_.copy(modalType = modalType, treeItem = treeItem, isModalOpen = true, dispatch = newDispatch, path = newPath, elemToModal = newElemToAdd))

    def openDollarModal = $.modState(_.copy(isDollarModalOpen = true))
    def openReleaseModal = $.modState(_.copy(isReleaseModal = true))

    def render(P: Props, S: State) = {
      val sc = AppCircuit.connect(_.tree)

      val send: Option[Callback] =
        for (websocket <- S.websocket if S.message.nonEmpty)
          yield sendMessage(websocket, S.message)

      val sendVerify: Option[Callback] =
        for (websocket <- S.websocket if P.proxy.value.toString.nonEmpty)
          yield sendMessage(websocket, P.proxy.value.makeString.replaceAll("\n", ""))


      def sendPrepMessage(prepMessage: String => String): Option[Callback] =
        for (websocket <- S.websocket if P.proxy.value.toString.nonEmpty)
          yield sendMessage(websocket, prepMessage(v1 = P.proxy.value.makeString.replaceAll("\n", "")))

//      val sendGetTemplate(templateNbr: Int): Option[Callback] =
//        for (websocket <- state.websocket if state.message.nonEmpty)
//          yield sendMessage(websocket, "Template" + templateNbr)



      def handleKeyDown(event: ReactKeyboardEventI): Option[Callback] = {
        if (event.nativeEvent.keyCode == KeyCode.Enter)
          send
        else
          None
      }

      def saveModel(P: Props, S: State): Callback = {
        if(S.cachedModels.size > 4)
          $.modState(_.copy(cachedModels = S.cachedModels.dequeue._2 :+ P.proxy.value))
        else
          $.modState(_.copy(cachedModels = S.cachedModels :+ P.proxy.value))
      }

      <.div(
        Modal(S.isModalOpen, closeModal, S.modalType, S.treeItem, S.dispatch, S.path, S.elemToModal),
        HundredDollarModal(isOpen = S.isDollarModalOpen, onClose = closeDollarModal, sendPrepMessage),
        ReleaseModal(isOpen = S.isReleaseModal, onClose = closeReleaseModal, sendPrepMessage, P.proxy.value),
        ^.className := "container",
        ^.width := "100%",
        ^.height := "100%",
        ^.paddingTop := "25px",
        ^.overflow := "hidden",
        <.div(
          ^.className := "col-1",
          ^.float := "left",
          ^.width := "29%",
          ^.height := "100%",
          ^.paddingRight := "9px",
          entityComponent(S),
          <.pre(
            ^.height := "40%",
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
                ^.className := "btn btn-default",
                ^.onClick --> P.proxy.dispatchCB(SetModel(S.latestRecTree.children)),
                "asdf"),
              <.button(
                ^.disabled := sendVerify.isEmpty,
                ^.className := "btn btn-default",
                "Verify Model",
                ^.onClick -->? sendVerify
              )
            ),
            log(S.logLines)// Display log
          ),
          cachedModels((P,S))),
        <.div(
          <.button(
            ^.className := "btn btn-default",
            "Copy Model",
            ^.onClick --> openModalWithContent(Modal.COPY_MODAL, elemToTreeItem(P.proxy.value.children), P.proxy.dispatchCB, Seq(P.proxy.value.makeString), None)
          ),
          <.button(
            ^.className := "btn btn-default",
            "Save Model",
            ^.onClick --> saveModel(P,S)
          ),
          <.button(
            ^.className := "btn btn-default",
            "100$ Dollar",
            ^.onClick --> openDollarModal
          ),
//          <.button(
//            ^.className := "btn btn-default",
//            "Ordinal Ranking",
//            ^.onClick --> openDollarModal
//          ),
          <.button(
            ^.className := "btn btn-default",
            "Release Planning",
            ^.onClick --> openReleaseModal
          )
        ),
        sc(proxy => treeView((proxy, openModalWithContent)))
      )
    }


    val entityComponent = ReactComponentB[State]("entityComponent")
      .render($ =>
        <.pre(
          Styles.dragList,
          <.form(
            <.input.text(
              ^.className := "form-control",
              ^.placeholder := "Search",
              ^.onChange ==> onTextChange
            )
          ),
          checkBoxes($.props),
          entityListView($.props.elems)
        )
      )
      .build

    val checkBoxes = ReactComponentB[State]("checkBoxes")
      .render( $ =>
        <.div(
          <.input.checkbox(
            ^.onChange ==> toggleEntity($.props)
          ),
          " Entity ",
          <.input.checkbox(
            ^.onChange ==> toggleAttribute($.props)
          ),
          " Attribute "
        ))
      .build

    val cachedModels = ReactComponentB[(Props, State)]("cachedModelsComponent")
      .render($ => <.pre(
        ^.className := "form-control",
        ^.overflow := "hidden",
        ^.height := "22%",
        $.props._2.cachedModels.reverse.map(s => listModels((s, $.props._1, $.props._2)))
      )
      ).build

    val listModels = ReactComponentB[(Tree, Props, State)]("listElem")
      .render(T => <.ul(
        T.props._1.toString.take(28),
        ^.boxShadow := "0px 6px 12px 0px rgba(0,0,0,0.2)",
        ^.padding := 5.px,
        ^.borderRadius := "5px",
        ^.background := "#CFEADD",
        ^.onDblClick --> T.props._2.proxy.dispatchCB(SetTemplate(T.props._1)),
        <.button(
          Styles.removeButtonSimple,
          ^.onClick --> removeCachedModel(T.props._3, T.props._1)
      )
      ))
      .build

    def removeCachedModel(state: State, tree: Tree): Callback = {
      val index = state.cachedModels.indexWhere(_.equals(tree))
      val beginning = state.cachedModels.take(index)
      val end = state.cachedModels.drop(index+1)

      $.modState(_.copy(cachedModels = beginning ++ end))
    }

    def toggleEntity(S: State)(event: ReactEventI): Callback = {
      if(S.entityChecked && S.attributeChecked)
        $.modState(_.copy(elems = attributes, entityChecked = !S.entityChecked))
      else if(S.attributeChecked || S.entityChecked)
        $.modState(_.copy(elems = elems, entityChecked = !S.entityChecked))
      else
        $.modState(_.copy(elems = entities, entityChecked = !S.entityChecked))
    }

    def toggleAttribute(S: State)(event: ReactEventI): Callback = {
      if(S.attributeChecked && S.entityChecked)
        $.modState(_.copy(elems= entities, attributeChecked = !S.attributeChecked))
      else if(S.attributeChecked || S.entityChecked)
        $.modState(_.copy(elems= elems, attributeChecked = !S.attributeChecked))
      else
        $.modState(_.copy(elems = attributes, attributeChecked = !S.attributeChecked))
    }

    val log = ReactComponentB[Vector[String]]("log")
      .render($ =>
        <.pre(
          ^.className := "form-control",
          ^.width := "auto",
          ^.height := "80%",
          ^.marginTop := "5px",
          ^.overflowY.auto,
          ^.whiteSpace.`pre-line`,
          $.props.map(<.p(_)))
      )
      .build

    def onTextChange(event: ReactEventI): Callback =
      event.extract(_.target.value.toLowerCase) {
        case "entity" | "entities" => $.modState(_.copy(elems = entities))
        case "attribute" | "attributes" => $.modState(_.copy(elems = attributes))
        case value => $.modState(_.copy(elems = elems.filter(_.toLowerCase.contains(value.toLowerCase))))
      }

    def onChange(event: ReactEventI): Callback = {
      val newMessage = event.target.value
      $.modState(_.copy(message = newMessage))
    }


    def sendMessage(websocket: WebSocket, msg: String): Callback = {
      def send = Callback(websocket.send(msg))
      def updateState = $.modState(s => s.log(s"Sent: \n$msg").copy(message = ""))

      send >> updateState
    }

    def start: Callback = {

      // This will establish the connection and return the WebSocket
      def connect = CallbackTo[WebSocket] {

        // Get direct access so WebSockets API can modify state directly
        // (for access outside of a normal DOM/React callback).
        val direct = $.accessDirect

        def onopen(event: Event): Unit = {
          direct.modState(_.log("Connected."))
        }


        def onmessage(event: MessageEvent): Unit = {
          if(event.data.toString.startsWith("{")){
            val tree = fixInputModel(read[Model](event.data.toString).tree)
            direct.modState(_.saveTree(tree))
          } else {
            direct.modState(_.log(s"${event.data.toString}"))
          }
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
    .initialState(State(None, Vector.empty, message = "", elems, isModalOpen = false, modalType = Modal.EMPTY_MODAL, treeItem = null))
    .renderBackend[Backend]
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.end)
    .build

  val dc = AppCircuit.connect(_.tree)

  def main(): Unit = {
    Styles.addToDocument()
    ReactDOM.render(dc(proxy => navigationBar((headerButtons, Props(proxy)))), document.getElementById("header"))
    ReactDOM.render(dc(proxy => pageContent(Props(proxy))), document.getElementById("content"))
  }
}
