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
import scala.scalajs.js.URIUtils._
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

//  case class modelProps(modalType: ModalType, treeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String], newElemToAdd: Option[Elem])

  case class Props(proxy: ModelProxy[Tree])

  case class CachedModel(name: String, model: Tree, selected: Boolean, uUID: UUID)

  case class OpenModals(isModalOpen: Boolean = false, isNewModelModalOpen: Boolean = false, isDollarModalOpen: Boolean = false,
                        isReleaseModalOpen: Boolean = false, isOrdinalModalOpen: Boolean = false)

  case class State(websocket: Option[WebSocket], logLines: Vector[String], message: String, elems: Seq[String], modalType: ModalType, treeItem: TreeItem,
                   dispatch: (Action => Callback) = null, path: Seq[String] = Seq(), elemToModal: Option[Elem] = None, entityChecked : Boolean = false,
                   attributeChecked: Boolean = false, cachedModels: Queue[CachedModel] = Queue(CachedModel("untitled", Tree(Seq()), selected = true, uUID = UUID.random())),
                   openModals: OpenModals = OpenModals(), saveModelType : String = "rec", latestRecTree: Tree = Tree(Seq()), isMethodStarted: Boolean = false,
                   waitingForModel: Boolean = false, scrollPosition: Double = 0
                  ) {


    def log(line: String): State = copy(logLines = logLines :+ line)

    def saveTree(tree: Tree): State = copy(latestRecTree = tree)

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



  import org.scalajs.dom
  import scala.scalajs.js.timers._
  def downloadModel(P: Props, S: State): Unit = {
    var file = new Blob(js.Array(P.proxy.value.makeString), js.Dynamic.literal(`type` = "text/plain").asInstanceOf[BlobPropertyBag])
    val a = document.createElement("a")
    var tempURL = dom.URL.createObjectURL(file)
    a.setAttribute("href", tempURL)

    if(S.cachedModels.exists(x => x.selected)){
      a.setAttribute("download", s"${S.cachedModels.find(x => x.selected).get.name}.txt")
    } else {
      a.setAttribute("download", "reqTModel.txt")
    }
    document.body.appendChild(a)
    a.asInstanceOf[dom.raw.HTMLBodyElement].click()
    setTimeout(1000) {
      document.body.removeChild(a)
      dom.URL.revokeObjectURL(tempURL)
    }
  }

  class Backend($: BackendScope[Props, State]) {

    def closeModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isModalOpen = false)))
    def closeDollarModal(e: ReactEvent): Callback =  $.modState(S => S.copy(openModals = S.openModals.copy(isDollarModalOpen = false)))
    def closeReleaseModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isReleaseModalOpen = false)))
    def closeOrdinalModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isOrdinalModalOpen = false)))
    def closeNewModelModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isNewModelModalOpen = false)))


    def openModalWithContent(modalType: ModalType, treeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String], newElemToAdd: Option[Elem] ): Callback
    = $.modState(_.copy(modalType = modalType, treeItem = treeItem, openModals = OpenModals(isModalOpen = true), dispatch = newDispatch, path = newPath, elemToModal = newElemToAdd))

    def openDollarModal = $.modState(_.copy(openModals = OpenModals(isDollarModalOpen = true)))
    def openOrdinalModal = $.modState(_.copy(openModals = OpenModals(isOrdinalModalOpen = true)))
    def openReleaseModal = $.modState(_.copy(openModals = OpenModals(isReleaseModalOpen = true)))
    def openNewModelModal(newSaveModelType: String) = $.modState(_.copy(openModals = OpenModals(isNewModelModalOpen = true),
      saveModelType = newSaveModelType))

    def setActiveModel(model: CachedModel,P: Props, S: State): Callback = {
      updateActiveModel(model, P,S)
    }

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
      )
      .build

    def setScroll(position : Double): Callback = {
      var temp = document.getElementById("treeView").asInstanceOf[dom.html.Pre]
      Callback(temp.scrollTop = temp.scrollHeight)
    }
    def getScroll: Callback ={
      $.modState(_.copy(scrollPosition = document.getElementById("treeView").scrollTop))
    }


    def updateActiveModel(model: CachedModel, P: Props, S: State): Callback = {
      val newModels: Queue[CachedModel] = S.cachedModels.map(mo =>
        if(mo.selected) {
          if(mo.uUID.equals(model.uUID))
            mo.copy(model = P.proxy.value, selected = true)
          else
            mo.copy(model = P.proxy.value, selected = false)
        } else if(mo.uUID.equals(model.uUID))
          mo.copy(selected = true)
        else mo
      )

      $.modState(_.copy(cachedModels = newModels))
    }

    def saveModel(name: String, model: Tree, P: Props): Callback =
      $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, model, selected = false, UUID.random())))


//    def saveModel(name: String, isCurrModel: Boolean, saveModelType: String, P: Props): Callback = {
//      println(name + "  " + saveModelType)
//
//      saveModelType match {
//        case "rec"  =>
//          if(isCurrModel)
//            $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, s.latestRecTree, selected = false, UUID.random())))
//          else
//            $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, Tree(Seq[Elem]()), selected = false, UUID.random())))
//
//        case "save" =>
//          if(isCurrModel)
//            $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, P.proxy.value, selected = false, UUID.random())))
//          else
//            $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, Tree(Seq[Elem]()), selected = false, UUID.random())))
//
//        case "imp" =>
//          if(isCurrModel)
//            $.modState(s => s.copy(cachedModels = s.cachedModels:+ CachedModel(name, P.proxy.value, selected = false, UUID.random())))
//          else
//            $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, Tree(Seq[Elem]()), selected = false, UUID.random())))
//
//        case "temp" => $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, P.proxy.value, selected = false, UUID.random())))
//
//        case _ => Callback()
//
//
//      }
//    }




    def render(P: Props, S: State) = {
      val sc = AppCircuit.connect(_.tree)

      val send: Option[Callback] ={
        for (websocket <- S.websocket if S.message.nonEmpty)
          yield sendMessage(websocket, S.message)
      }

      val sendVerify: Option[Callback] =
        for (websocket <- S.websocket if P.proxy.value.toString.nonEmpty)
          yield sendMessage(websocket, P.proxy.value.makeString.replaceAll("\n", ""))


      def sendPrepMessage(prepMessage: String => Seq[String]): Option[Seq[Callback]] = {
        for (websocket <- S.websocket if P.proxy.value.toString.nonEmpty)
          yield sendMessages(websocket, prepMessage(v1 = P.proxy.value.makeString.replaceAll("\n", ""))) :+ setMethodStarted
      }

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
        HundredDollarModal(isOpen = S.openModals.isDollarModalOpen, onClose = closeDollarModal, sendPrepMessage),
        ReleaseModal(isOpen = S.openModals.isReleaseModalOpen, onClose = closeReleaseModal, sendPrepMessage, P.proxy.value),
        OrdinalModal(isOpen = S.openModals.isOrdinalModalOpen, onClose = closeOrdinalModal, sendPrepMessage, P.proxy.value),
        NewModelModal(isOpen = S.openModals.isNewModelModalOpen, onClose = closeNewModelModal, saveModel = saveModel(_, _, P),
          {if (S.saveModelType.equals("rec") || S.saveModelType.equals("temp")) S.latestRecTree else P.proxy.value}, S.saveModelType),
        ^.className := "container",
        ^.width := "100%",
        ^.height := "100%",
        ^.overflow := "hidden",
        ^.paddingRight := "5px",
        ^.paddingLeft := "5px",
        <.div(
          ^.className := "header",
          navigationBar((headerButtons, P, S))
        ),
        <.div(
          ^.className := "col-1",
          ^.float := "left",
          ^.width := "29%",
          ^.height := "100%",
          ^.paddingRight := "9px",
          entityComponent(S),
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
          sc(proxy => treeView((proxy, openModalWithContent)))
        )
      )



    }

    val headerButtons = Seq("Import", "Export", "Copy Model", "Templates", "100$", "Ordinal", "Release", "Help")

    val buttonComponent = ReactComponentB[(String, Props, State)]("buttonComponent")
      .render($ =>
        $.props._1 match {

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
          case "Export" =>
            <.button(
              Styles.navBarButton,
              "Export",
              ^.onClick --> Callback(downloadModel($.props._2, $.props._3))
            )
          case "Copy Model" =>
            <.button(
              Styles.navBarButton,
              "Copy Model",
              ^.onClick --> openModalWithContent(Modal.COPY_MODAL,
                elemToTreeItem($.props._2.proxy.value.children), $.props._2.proxy.dispatchCB, Seq($.props._2.proxy.value.makeString), None)
            )
          case "Templates" => TemplateSelect(setTemplate, openNewModelModal)
          case "100$" =>
            <.button(
              Styles.navBarButton,
              "100$",
              ^.onClick --> openDollarModal
            )
          case "Release" =>
            <.button(
              Styles.navBarButton,
              "Release",
              ^.onClick --> openReleaseModal
            )
          case "Ordinal" =>
            <.button(
            Styles.navBarButton,
            "Ordinal Ranking",
            ^.onClick --> openOrdinalModal
          )
          case "Help" =>
            <.button(
              Styles.navBarButton,
              ^.className := "glyphicon glyphicon-question-sign pull-right"
            )
          case _ => <.button(
            $.props._1,
            Styles.navBarButton,
            ^.onClick --> Callback()
          )
        }).build

    def setTemplate(child: Seq[Elem]): Callback = $.modState(_.copy(latestRecTree = Tree(child)))


    val navigationBar = ReactComponentB[(Seq[String], Props, State)]("navigationBar")
      .render($ => <.nav(
        ^.paddingLeft := "15px",
        ^.paddingRight := "15px",
        Styles.navBar,
        $.props._1.map(x => buttonComponent((x, $.props._2, $.props._3)))
      )
      ).build


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
          ^.onClick --> openNewModelModal("save")
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
        ^.onClick --> (setActiveModel($.props._1, $.props._2, $.props._3) >> $.props._2.proxy.dispatchCB(SetModel($.props._1.model.children))),

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

    def activeModel(activeModel: CachedModel, cachedModels: Queue[CachedModel]): Callback = $.modState(_.copy(cachedModels = cachedModels.map(
      x => if(x.uUID.equals(activeModel.uUID)) x.copy(selected = true) else x.copy(selected = false))))

    def parseModel(newModel: String, dispatch: Action => Callback): Unit = {
      Ajax.get("/getmodelfromstring/" + encodeURI(newModel.trim.replaceAll(" +", " "))).onComplete{
        case Success(r) =>
          dispatch(SetModel(fixInputModel(read[Model](r.responseText).tree).children)).runNow()
        case Failure(e) =>
          println(e.toString)
      }
    }

    def importModel(dispatch: Action => Callback)(e: ReactEventI): Callback = {
      if(e.currentTarget.files.item(0).`type` == "text/plain") {
        var newModel = "newModel empty, shouldn't happen"
        val fileReader = new FileReader
        fileReader.readAsText(e.currentTarget.files.item(0), "UTF-8")

        fileReader.onload = (_: UIEvent) => {
          newModel = fileReader.result.asInstanceOf[String]
          parseModel(newModel.replace("\n", "").trim, dispatch)
          openNewModelModal("imp").runNow()
        }
        Callback(e.currentTarget.value = "")
      } else {
        Callback(window.alert("Invalid file type, only .txt is supported"))
      }
    }

    def toggleActiveTab(model: CachedModel, cachedModels: Queue[CachedModel]): Callback = {
      Callback()
    }

    def removeCachedModel(state: State, modelToRemove: CachedModel)(e: ReactEventI): Callback = {
      e.stopPropagation()
      val index = state.cachedModels.indexWhere(_.equals(modelToRemove))
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
          ^.id := "reqTLog",
          ^.width := "auto",
          ^.contentEditable := "true",
          ^.height := "80%",
          ^.marginTop := "5px",
          ^.overflowY.auto,
          ^.overflowX.hidden,
          ^.whiteSpace.`pre-line`,
          $.props.map(<.p(_)))
      )
      .componentDidUpdate(_ => updateScroll)
      .build

    def updateScroll: Callback = {
      var reqtLog = document.getElementById("reqTLog").asInstanceOf[dom.html.Pre]
      Callback({
        reqtLog
        reqtLog.scrollTop = reqtLog.scrollHeight
      })
    }

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
        openNewModelModal("rec").runNow()
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
    .initialState(State(None, Vector.empty, message = "", elems, modalType = Modal.EMPTY_MODAL, treeItem = null))
    .renderBackend[Backend]
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.end)
    .build

  val dc = AppCircuit.connect(_.tree)

  def main(): Unit = {
    Styles.addToDocument()
    ReactDOM.render(dc(proxy => pageContent(Props(proxy))), document.getElementById("content"))
  }
}
