package example

import org.scalajs.dom._
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react._
import diode.react.ModelProxy
import modals.NewModelModal

import scalacss.ScalaCssReact._
import scalacss.Defaults._
import scala.collection.immutable.Queue
import shared._

@JSExport
object webApp extends js.JSApp {

  val contentDivStyle = Seq(
    ^.className := "container",
    ^.width := "100%",
    ^.height := "100%",
    ^.overflow := "hidden",
    ^.paddingRight := "5px",
    ^.paddingLeft := "5px"
  )

  val ListTerminalDivStyle = Seq(
    ^.className := "col-1",
    ^.float := "left",
    ^.width := "29%",
    ^.height := "100%",
    ^.paddingRight := "9px"
  )

  val cachedModelsDivStyle = Seq(
    ^.className := "col-2",
    ^.width := "71%",
    ^.height := "100%",
    ^.float.left
  )

  val cachedModelsPreStyle = Seq(
    ^.padding := "5px",
    ^.paddingRight := "5px",
    ^.height := "5%",
    ^.overflow := "hidden",
    ^.position.relative
  )

  val addCachedModelsButtonStyle = Seq(
    ^.className := "glyphicon glyphicon-plus",
    ^.color := "green",
    ^.position.absolute,
    ^.top := "0%",
    ^.width := "5%",
    ^.height := "105%",
    ^.marginLeft := "-6px",
    ^.marginTop := "-2px",
    ^.outline := "none"
  )

  val cachedModelsDiv1Style = Seq(
    ^.overflowX.auto,
    ^.left := "5%",
    ^.height := "91%",
    ^.overflowX.auto,
    ^.overflowY.hidden,
    ^.width := "95%",
    ^.position.absolute
  )

  val modelTabsStyle = Seq(
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
    ^.background := "#CFEADD"
  )

  val modelTabsSpanStyle = Seq(
    ^.className := "col",
    ^.position.absolute,
    ^.width := "80%",
    ^.height := "30px",
    ^.paddingTop := "2px",
    ^.textAlign := "center"
  )

  val modelTabsButtonStyle = Seq(
    ^.className := "col",
    ^.position.absolute,
    ^.width := "20%",
    ^.height := "30px",
    ^.left := "80%",
    ^.top := "0%",
    ^.paddingTop := "5px"
  )

  val cachedModelsRowStyle = Seq(
    ^.whiteSpace := "nowrap",
    ^.position.absolute,
    ^.className := "clickable-row"
  )

  case class Props(proxy: ModelProxy[Tree])

  case class CachedModel(name: String, model: Tree, selected: Boolean, uUID: UUID)

  case class State(cachedModels: Queue[CachedModel] = Queue(CachedModel("untitled", emptyTree, selected = true, uUID = UUID.random())),
                   isNewModelModalOpen: Boolean = false, saveModelType: String = "rec",
                   isMethodStarted: Boolean = false, scrollPosition: Double = 0, newModel: Tree = emptyTree, method: Seq[String] = Seq())

  val emptyTree = Tree(Seq())

  def elemToTreeItem(elems: Seq[Elem]): TreeItem = {
    TreeItem("Model", UUID.model(), elems.map(elem => convert(elem)), None)
  }

  def convert(elem: Elem): TreeItem = elem match {
    case relation: Relation => TreeItem(relation.entity, relation.entity.uuid, relation.submodel.children.map(convert), Some(relation.link))
    case node: shared.Node => TreeItem(node, node.uuid, Seq(), None)
  }


  class Backend($: BackendScope[Props, State]) {

    def saveScrollPosition(position: Double): Callback = {
      if ($.accessDirect.state.scrollPosition != position)
        $.modState(_.copy(scrollPosition = position))
      else
        Callback()
    }

    def closeNewModelModal: Callback = $.modState(_.copy(isNewModelModalOpen = false))

    def openNewModelModal(newSaveModelType: String, newModel: Tree): Callback = $.modState(_.copy(isNewModelModalOpen = true,
      saveModelType = newSaveModelType, newModel = newModel))


    val treeView = ReactComponentB[ModelProxy[Tree]]("treeView")
      .render(P => <.pre(
        ^.className := "zoomViewport",
        Styles.treeView,
        ^.overflowX := "hidden",
        ^.border := "1px solid #ccc",
        ^.id := "treeView",
        <.div(
          ^.width := "100%",
          ^.height := "100%",
          ReactTreeView(
            root = elemToTreeItem(P.props.value.children),
            modelProxy = P.props,
            showSearchBox = false,
            saveScrollPosition = saveScrollPosition
          ),
          <.strong(
            ^.id := "treeviewcontent"
          )
        )
      ))
      .build

    def setScroll(scrollPosition: Double): Callback = {
      val pre = document.getElementById("treeView").asInstanceOf[dom.html.Pre]
      Callback(pre.scrollTop = scrollPosition)
    }

    def getScroll: Callback = $.modState(_.copy(scrollPosition = document.getElementById("treeView").scrollTop))


    def saveModel(name: String, model: Tree, P: Props): Callback =
      $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, model, selected = false, UUID.random())))

    def sendMethod(currentMethod: Seq[String]) = $.modState(_.copy(method = currentMethod, isMethodStarted = true))

    def methodDone = $.modState(_.copy(isMethodStarted = false))

    def render(P: Props, S: State) = {
      val sc = AppCircuit.connect(_.tree)
      <.div(
        NewModelModal(isOpen = S.isNewModelModalOpen, onClose = closeNewModelModal, saveModel = saveModel(_, _, P), S.newModel, S.saveModelType),
        contentDivStyle,
        <.div(
          ^.className := "header",
          Header(P.proxy, openNewModelModal, sendMethod, getActiveModelName)
        ),
        <.div(
          ListTerminalDivStyle,
          ElementList()
          //ReqTLog(P.proxy, openNewModelModal, () => S.method, S.isMethodStarted, methodDone)
        ),
        <.div(
          cachedModelsDivStyle,
          cachedModels((P, S)),
          sc(proxy => treeView(proxy))
        )
      )
    }

    val cachedModels = ReactComponentB[(Props, State)]("cachedModelsComponent")
      .render($ => <.pre(
        cachedModelsPreStyle,
        <.button(
          addCachedModelsButtonStyle,
          ^.onClick --> openNewModelModal("save", $.props._1.proxy.value)
        ),
        <.div(
          cachedModelsDiv1Style,
          <.div(
            cachedModelsRowStyle,
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
        modelTabsStyle,
        ^.opacity := {
          if ($.props._1.selected) "1" else "0.7"
        },
        <.span(
          modelTabsSpanStyle,
          $.props._1.name
        ),
        ^.onClick --> setActiveModel($.props._1, $.props._2, $.props._3),

        <.button(
          modelTabsButtonStyle,
          Styles.removeButtonSimple,
          ^.outline := "none",
          ^.onClick ==> removeCachedModel($.props._1, $.props._2, $.props._3)
        )
      )).build

    def getActiveModelName: Option[CachedModel] = $.accessDirect.state.cachedModels.find(_.selected)


    def setActiveModel(cachedModel: CachedModel, P: Props, S: State): Callback = {
      updateActiveModel(cachedModel, P, S) >> P.proxy.dispatchCB(SetModel(cachedModel.model.children))
    }

    def updateActiveModel(cachedModel: CachedModel, P: Props, S: State): Callback = {
      val newModels: Queue[CachedModel] = S.cachedModels.map(model =>
        if (model.selected)
          model.copy(model = P.proxy.value, selected = model.uUID.equals(cachedModel.uUID))
        else
          model.copy(selected = model.uUID.equals(cachedModel.uUID))
      )
      $.modState(_.copy(cachedModels = newModels))
    }

    def removeCachedModel(modelToRemove: CachedModel, P: Props, S: State)(e: ReactEventI): Callback = {
      e.stopPropagation()
      val index = S.cachedModels.indexWhere(_.equals(modelToRemove))
      val beginning = S.cachedModels.take(index)
      val end = S.cachedModels.drop(index + 1)

      $.modState(_.copy(cachedModels = beginning ++ end))
    }

  }

  val pageContent = ReactComponentB[Props]("Content")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillReceiveProps(x => x.$.backend.setScroll(x.currentState.scrollPosition))
    .componentDidUpdate(x => {
      x.$.backend.setScroll(x.currentState.scrollPosition)
    })
    .build

  val dc = AppCircuit.connect(_.tree)

  def main(): Unit = {
    Styles.addToDocument()
    ReactDOM.render(dc(proxy => pageContent(Props(proxy))), document.getElementById("content"))
  }
}
