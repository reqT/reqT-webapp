package example

import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.CompScope._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^.{^, _}
import org.scalajs.dom.{console, document}

import scala.scalajs.js.Dynamic.{global => global}
import scalacss.ScalaCssReact._
import scala.scalajs.js


case class TreeItem(var item: Any, var children: Seq[TreeItem]) {
  def apply(item: Any): TreeItem = this (item, Seq())
}

object ReactTreeView {

  trait Style {

    def reactTreeView = Seq[TagMod]()

    def treeGroup = Seq(^.margin := 0, ^.padding := "0 0 0 40px")

    def treeItem = Seq(^.listStyleType := "none")

    def selectedTreeItemContent = Seq(^.backgroundColor := "#1B8EB0",
      ^.color := "white", ^.fontWeight := 400,
      ^.padding := "0 40px")

    def treeItemBefore = Seq(
      ^.position := "absolute",
      ^.height := "100.%%",
      ^.left := "2.%%",
      ^.display := "inline-block",
      ^.fontSize := "14px",
      ^.color := "grey",
      //      ^.margin := "3px 7px 0 0",
      ^.textAlign := "center",
      ^.width := "11px"
    )

    def treeItemHasChildrenClosed = Seq(^.contentStyle := "▶")

    def treeItemHasChildrenOpened = Seq(^.contentStyle := "▼")

  }

  type NodeC = DuringCallbackU[NodeProps, NodeState, NodeBackend]

  case class State(filterText: String,
                   filterMode: Boolean,
                   selectedNode: js.UndefOr[NodeC])

  class Backend($: BackendScope[Props, State]) {

    def onNodeSelect(P: Props)(selected: NodeC): Callback = {
      val removeSelection: Callback =
        $.state.flatMap(
          _.selectedNode
            .filterNot(_ == selected)
            .filter(_.isMounted())
            .fold(Callback.empty)(_.modState(_.copy(selected = false)))
        )

      val updateThis: Callback =
        $.modState(_.copy(selectedNode = selected, filterMode = false))

      val setSelection: Callback =
        selected.modState(_.copy(selected = true))

      val tell: Callback = Callback(println(selected.props.root.item.toString))
      /*     P.onItemSelect.asCbo(
             selected.props.root.item.toString,
             selected.props.parent,
             selected.props.depth
           )*/

      removeSelection >> updateThis >> setSelection >> tell
    }

    def onTextChange(text: String): Callback =
      $.modState(_.copy(filterText = text, filterMode = true))

    def render(P: Props, S: State) =
      <.div(P.style.reactTreeView)(
        //P.showSearchBox ?= ReactSearchBox(onTextChange = onTextChange),
        TreeNode.withKey("root")(NodeProps(
          root = P.root,
          open = if (S.filterText.nonEmpty) true else P.open,
          onNodeSelect = onNodeSelect(P),
          filterText = S.filterText,
          style = P.style,
          filterMode = S.filterMode,
          modelProxy = P.modelProxy
        ))
      )
  }

  case class NodeBackend($: BackendScope[NodeProps, NodeState]) {
    import upickle.default.read
    def dragStart(P: NodeProps)(event: ReactDragEvent): Callback = {
      val path = if (P.parent.isEmpty) P.root.item.toString
      else P.parent + "/" + P.root.item

      if(path != "Model()"){
        event.dataTransfer.effectAllowed = "move"
        event.dataTransfer.setData("existing", "true")
        event.dataTransfer.setData("path", path)
        Callback()
      }else{
        Callback()
      }

    }

    def getElem(treeItem: TreeItem): Elem = {
      if (treeItem.children.isEmpty)
        treeItem.item.asInstanceOf[Elem]
      else {
        Relation(treeItem.item.asInstanceOf[Entity], has, Tree(treeItem.children.map(getElem)))
      }
    }

    def onTreeMenuToggle(P: NodeProps)(e: ReactEventH): Callback =
      childrenFromProps(P) >> e.preventDefaultCB >> e.stopPropagationCB

    def onItemSelect(P: NodeProps)(e: ReactEventH): Callback =
      P.onNodeSelect($.asInstanceOf[NodeC]) >> childrenFromProps(P) >> e.preventDefaultCB >> e.stopPropagationCB

    def childrenFromProps(P: NodeProps): CallbackTo[Option[Unit]] =
      $.modState(S => S.copy(children = if (S.children.isEmpty) P.root.children else Nil))
        .conditionally(P.root.children.nonEmpty)


    def isFilterTextExist(filterText: String, data: TreeItem): Boolean = {
      def matches(item: TreeItem): Boolean =
        item.item.toString.toLowerCase.contains(filterText.toLowerCase)

      def loop(data: Seq[TreeItem]): Boolean =
        data.view.exists(
          item => if (item.children.isEmpty) matches(item) else loop(item.children)
        )

      matches(data) || loop(data.children)
    }

    def onDrop(P: NodeProps)(event: ReactDragEvent): Callback = {
      event.preventDefault()

      val pathFrom = event.dataTransfer.getData("path")
      val pathTo = (if (P.parent.isEmpty) P.root.item.toString
        else P.parent + "/" + P.root.item).split("/")
      val dispatch: Action => Callback = P.modelProxy.dispatchCB
      var isAttribute = false
      if (P.root.item.toString != "Model()") {
        isAttribute = P.root.item.asInstanceOf[Elem].isAttribute
      }


      def getElem(event: ReactDragEvent): Elem = event.dataTransfer.getData("type") match {
        case "entity" =>
          val elem = read[Entity](event.dataTransfer.getData("elem"))
          if (event.dataTransfer.getData("existing") == "false"){
            val newId = global.prompt("Input Entity ID").toString
            elem.setID(newId)
          }
          elem
        case "stringAttr" =>
          val elem = read[StringAttribute](event.dataTransfer.getData("elem"))
          if (event.dataTransfer.getData("existing") == "false"){
            val newValue = global.prompt("Input String Attribute Value").toString
            elem.setValue(newValue)
          }
          elem
        case "intAttr" =>
          val elem = read[IntAttribute](event.dataTransfer.getData("elem"))
          if (event.dataTransfer.getData("existing") == "false"){
            val newValue = global.prompt("Input Int Attribute Value").toString.toInt
            elem.setValue(newValue)
          }
          elem
      }


      if (event.dataTransfer.getData("existing") == "false") {
        dispatch(AddElem(pathTo, getElem(event)))
      } else
      if (isAttribute || pathFrom.split("/").diff(pathTo).isEmpty) { // Hindra att dra till Attribute och sina egna children
        dispatch(NoAction)
      } else {
        dispatch(RemoveElem(pathFrom.split("/"))) >> dispatch(MoveElem(pathFrom.split("/"), pathTo))
      }
    }


    def dragOver(e: ReactDragEvent): Callback = {
      e.preventDefault()
      e.dataTransfer.dropEffect = "move"
      Callback()
    }

    def removeElem(P: NodeProps): Callback = {
      val dispatch: Action => Callback = P.modelProxy.dispatchCB
      val path = if (P.parent.isEmpty) P.root.item.toString
      else P.parent + "/" + P.root.item
      dispatch(RemoveElem(path.split("/")))
    }

    def render(P: NodeProps, S: NodeState): ReactTag = {
      val dispatch: Action => Callback = P.modelProxy.dispatchCB
      val depth = P.depth + 1
      val parent = if (P.parent.isEmpty) P.root.item.toString
      else s"${P.parent}/${P.root.item.toString}"


      val treeMenuToggle: TagMod =
        if (S.children.nonEmpty)
          <.span(
            ^.onClick ==> onTreeMenuToggle(P),
            ^.key := "arrow",
            P.style.treeItemBefore,
            "▼"
          )
        else if (P.root.children.nonEmpty && S.children.isEmpty)
          <.span(
            ^.onClick ==> onTreeMenuToggle(P),
            ^.key := "arrow",
            P.style.treeItemBefore,
            "▶"
          )
        else ""

      <.li(
        P.style.treeItem,
        //        treeMenuToggle,
        //        ^.key := "toggle",
        //        ^.cursor := "pointer",
        <.div(
          ^.overflow.hidden,
          ^.position.relative,
          ^.border := "1px solid",
          ^.borderRadius := "5px",
          ^.backgroundColor := "#EDF2F4",
          ^.padding := "5px",
          ^.width := "400px",
          ^.height := "40px",
          treeMenuToggle,
          ^.key := "toggle",
          ^.cursor := "pointer",
          ^.className := "container",
          ^.onClick ==> onItemSelect(P),
          ^.draggable := true,
          ^.onDragStart ==> dragStart(P),
          ^.onDrop ==> onDrop(P),
          ^.onDragOver ==> dragOver,
          <.span(
            ^.id := P.root.item.toString,
            ^.unselectable := "true",
            P.root.item.toString,
            ^.position := "absolute",
            ^.left := "7%",
            ^.top := "25%",
            ^.fontSize := "large"
//            ^.onClick ==> onItemSelect(P),
//            ^.draggable := true,
//            ^.onDragStart ==> dragStart(P),
//            ^.onDrop ==> onDrop(P),
//            ^.onDragOver ==> dragOver

          ),
          <.button(
            Styles.bootStrapContentButton
            //            ^.onClick --> ViewContent()
          ),
          <.button(
            Styles.bootStrapRemoveButton,
            ^.onClick --> removeElem(P)
          )


        ),
        <.ul(P.style.treeGroup)(
          S.children.map(child =>
            isFilterTextExist(P.filterText, child) ?=
              TreeNode.withKey(s"$parent/${child.item}")(P.copy(
                root = child,
                open = true, //!P.filterText.trim.isEmpty,
                depth = depth,
                parent = parent,
                filterText = P.filterText
              ))
          ))
      )
    }
  }

  case class NodeState(children: Seq[TreeItem] = Nil, selected: Boolean = false)

  case class NodeProps(root: TreeItem,
                       open: Boolean,
                       depth: Int = 0,
                       parent: String = "",
                       onNodeSelect: (NodeC) => Callback,
                       filterText: String,
                       style: Style,
                       filterMode: Boolean,
                       modelProxy: ModelProxy[Tree]
                      )

  lazy val TreeNode = ReactComponentB[NodeProps]("ReactTreeNode")
    .initialState_P(P => if (P.open) NodeState(P.root.children) else NodeState())
    .renderBackend[NodeBackend]
    .componentWillReceiveProps {
      case ComponentWillReceiveProps(_$, newProps) =>
        _$.modState(_.copy(children = if (newProps.open) newProps.root.children else Nil))
          .conditionally(newProps.filterMode)
          .void
    }
    .build

  val component = ReactComponentB[Props]("ReactTreeView")
    .initialState(State("", false, js.undefined))
    .renderBackend[Backend]
    .build

  case class Props(root: TreeItem,
                   open: Boolean,
                   onItemSelect: js.UndefOr[(String, String, Int) => Callback],
                   showSearchBox: Boolean,
                   style: Style,
                   modelProxy: ModelProxy[Tree]
                  )

  def apply(root: TreeItem,
            openByDefault: Boolean = false,
            onItemSelect: js.UndefOr[(String, String, Int) => Callback] = js.undefined,
            showSearchBox: Boolean = false,
            ref: js.UndefOr[String] = js.undefined,
            key: js.UndefOr[js.Any] = js.undefined,
            style: Style = new Style {},
            modelProxy: ModelProxy[Tree]
           ) =
    component.set(key, ref)(Props(root, openByDefault, onItemSelect, showSearchBox, style, modelProxy))

}