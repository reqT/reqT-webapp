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

@JSExport
object webApp extends js.JSApp {


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
                   waitingForModel: Boolean = false, scrollPosition: Double = 0) {

    def log(line: String): State = copy(logLines = logLines :+ line)

    def saveTree(tree: Tree): State = copy(latestRecTree = tree)

  }

  def elemToTreeItem(elems: Seq[Elem]): TreeItem = {
    TreeItem("Model", UUID.model(), elems.map(elem => convert(elem)), None)
  }

  def convert(elem: Elem): TreeItem = elem match {
    case relation: Relation => TreeItem(relation.entity, relation.entity.uuid, relation.submodel.children.map(convert), Some(relation.link))
    case node: shared.Node => TreeItem(node, node.uuid, Seq(), None)
  }

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

    def closeDollarModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isDollarModalOpen = false)))

    def closeReleaseModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isReleaseModalOpen = false)))

    def closeOrdinalModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isOrdinalModalOpen = false)))

    def closeNewModelModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isNewModelModalOpen = false)))


    def openModalWithContent(modalType: ModalType, treeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String], newElemToAdd: Option[Elem]): Callback
    = $.modState(_.copy(modalType = modalType, treeItem = treeItem, openModals = OpenModals(isModalOpen = true), dispatch = newDispatch, path = newPath, elemToModal = newElemToAdd))

    def openDollarModal = $.modState(_.copy(openModals = OpenModals(isDollarModalOpen = true)))

    def openOrdinalModal = $.modState(_.copy(openModals = OpenModals(isOrdinalModalOpen = true)))

    def openReleaseModal = $.modState(_.copy(openModals = OpenModals(isReleaseModalOpen = true)))

    def openNewModelModal(newSaveModelType: String): Callback = $.modState(_.copy(openModals = OpenModals(isNewModelModalOpen = true),
      saveModelType = newSaveModelType))

    def setActiveModel(model: CachedModel, P: Props, S: State): Callback = {
      updateActiveModel(model, P, S)
    }

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


    def setScroll(position: Double): Callback = {
      var temp = document.getElementById("treeView").asInstanceOf[dom.html.Pre]
      Callback(temp.scrollTop = temp.scrollHeight)
    }

    def getScroll: Callback = {
      $.modState(_.copy(scrollPosition = document.getElementById("treeView").scrollTop))
    }


    def updateActiveModel(model: CachedModel, P: Props, S: State): Callback = {
      val newModels: Queue[CachedModel] = S.cachedModels.map(mo =>
        if (mo.selected) {
          if (mo.uUID.equals(model.uUID))
            mo.copy(model = P.proxy.value, selected = true)
          else
            mo.copy(model = P.proxy.value, selected = false)
        } else if (mo.uUID.equals(model.uUID))
          mo.copy(selected = true)
        else mo
      )

      $.modState(_.copy(cachedModels = newModels))
    }

    def saveModel(name: String, model: Tree, P: Props): Callback =
      $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, model, selected = false, UUID.random())))


    def render(P: Props, S: State) = {
      val sc = AppCircuit.connect(_.tree)

      <.div(
        Modal(S.openModals.isModalOpen, closeModal, S.modalType, S.treeItem, S.dispatch, S.path, S.elemToModal),
        //        HundredDollarModal(isOpen = S.openModals.isDollarModalOpen, onClose = closeDollarModal, sendPrepMessage),
        //        ReleaseModal(isOpen = S.openModals.isReleaseModalOpen, onClose = closeReleaseModal, sendPrepMessage, P.proxy.value),
        //        OrdinalModal(isOpen = S.openModals.isOrdinalModalOpen, onClose = closeOrdinalModal, sendPrepMessage, P.proxy.value),
        //        NewModelModal(isOpen = S.openModals.isNewModelModalOpen, onClose = closeNewModelModal, saveModel = saveModel(_, _, P),
        //          {if (S.saveModelType.equals("rec") || S.saveModelType.equals("temp")) S.latestRecTree else P.proxy.value}, S.saveModelType),
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
          ElementList(),
          ReqTLog(P.proxy, stateSaveTree(_), openNewModelModal(_))
        ),
        <.div(
          ^.className := "col-2",
          ^.width := "71%",
          ^.height := "100%",
          ^.float.left,
          CachedModels(P.proxy),
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
              ^.`type` := "file",
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

    def stateSaveTree(tree: Tree): Unit = {
      $.accessDirect.state.saveTree(tree)
    }

    val navigationBar = ReactComponentB[(Seq[String], Props, State)]("navigationBar")
      .render($ => <.nav(
        ^.paddingLeft := "15px",
        ^.paddingRight := "15px",
        Styles.navBar,
        $.props._1.map(x => buttonComponent((x, $.props._2, $.props._3)))
      )
      ).build


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
        ^.opacity := {
          if ($.props._1.selected) "1" else "0.5"
        },

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
      x => if (x.uUID.equals(activeModel.uUID)) x.copy(selected = true) else x.copy(selected = false))))

    def parseModel(newModel: String, dispatch: Action => Callback): Unit = {
      Ajax.get("/getmodelfromstring/" + encodeURI(newModel.trim.replaceAll(" +", " "))).onComplete {
        case Success(r) =>
          dispatch(SetModel(fixInputModel(read[Model](r.responseText).tree).children)).runNow()
        case Failure(e) =>
          println(e.toString)
      }
    }

    def importModel(dispatch: Action => Callback)(e: ReactEventI): Callback = {
      if (e.currentTarget.files.item(0).`type` == "text/plain") {
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

    def removeCachedModel(state: State, modelToRemove: CachedModel)(e: ReactEventI): Callback = {
      e.stopPropagation()
      val index = state.cachedModels.indexWhere(_.equals(modelToRemove))
      val beginning = state.cachedModels.take(index)
      val end = state.cachedModels.drop(index + 1)
      $.modState(_.copy(cachedModels = beginning ++ end))
    }

  }
    val pageContent = ReactComponentB[Props]("Content")
      .initialState(State(None, Vector.empty, message = "", modalType = Modal.EMPTY_MODAL, treeItem = null))
      .renderBackend[Backend]
      .componentDidUpdate(_ => Callback(println("hej")))
      .build

    val dc = AppCircuit.connect(_.tree)

  def main(): Unit = {
    Styles.addToDocument()
    ReactDOM.render(dc(proxy => pageContent(Props(proxy))), document.getElementById("content"))
  }
}
