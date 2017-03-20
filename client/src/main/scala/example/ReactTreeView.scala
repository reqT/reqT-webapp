package example

import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.CompScope._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^.{^, _}

import scala.scalajs.js.Dynamic.{global => global}
import scalacss.ScalaCssReact._
import scala.scalajs.js


case class TreeItem(var item: Any, var children: Seq[TreeItem], var link: Option[RelationType]) {
  def apply(item: Any): TreeItem = this (item, Seq())
}

object ReactTreeView {
  trait Style {

    def reactTreeView = Seq[TagMod]()

    def treeGroup = Seq(^.margin := 0, ^.padding := "0 0 0 40px")

    def treeItem = Seq(^.listStyleType := "none")

    def selectedTreeItemContent = Seq(^.color := "blue")

    def dragOverTreeItemContent = Seq(^.opacity := 0.5)

    def treeItemBefore = Seq(
      ^.position := "absolute",
      ^.height := "100.%%",
      ^.left := "2.%%",
      ^.display := "inline-block",
      ^.fontSize := "14px",
      ^.color := "grey",
      ^.textAlign := "center",
      ^.width := "11px"
    )

    def treeItemHasChildrenClosed = Seq(^.contentStyle := "▶")

    def treeItemHasChildrenOpened = Seq(^.contentStyle := "▼")
  }

  def getRelationType(treeItem: TreeItem): Option[String] = {
    treeItem.item match {
      case "Model()" => None
      case elem : Elem  => if (elem.hasRelation) Some(" - " + treeItem.link.get.toString) else None
    }
  }

  type NodeC = DuringCallbackU[NodeProps, NodeState, NodeBackend]

  case class State(filterText: String,
                   filterMode: Boolean,
                   selectedNode: js.UndefOr[NodeC],
                   dragOverNode: js.UndefOr[NodeC])

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

      removeSelection >> updateThis >> setSelection //>> tell
    }

    def onNodeDrag(P: Props)(draggedOver: NodeC): Callback = {
      val removeCurrent: Callback =
        $.state.flatMap(
          _.dragOverNode
            .filterNot(_ == draggedOver)
            .filter(_.isMounted())
            .fold(Callback.empty)(_.modState(_.copy(draggedOver = false)))
        )

      val updateThis: Callback =
        $.modState(_.copy(dragOverNode = draggedOver, filterMode = false))

      val setCurrent: Callback =
        draggedOver.modState(_.copy(draggedOver = true))

      removeCurrent >> updateThis >> setCurrent
    }

    def onTextChange(text: String): Callback =
      $.modState(_.copy(filterText = text, filterMode = true))

    def render(P: Props, S: State) =
      <.div(P.style.reactTreeView)(
        P.showSearchBox ?= ReactSearchBox(onTextChange = onTextChange),
        TreeNode.withKey("root")(NodeProps(
          root = P.root,
          open = if (S.filterText.nonEmpty) true else P.open,
          onNodeSelect = onNodeSelect(P),
          onNodeDrag = onNodeDrag(P),
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

        event.dataTransfer.effectAllowed = "move"
        event.dataTransfer.setData("existing", "true")
        event.dataTransfer.setData("path", path)
        Callback()
    }

    def onTreeMenuToggle(P: NodeProps)(e: ReactEventH): Callback =
      childrenFromProps(P) >> e.preventDefaultCB >> e.stopPropagationCB

    def onItemSelect(P: NodeProps)(e: ReactEventH): Callback =
      P.onNodeSelect($.asInstanceOf[NodeC]) >> e.preventDefaultCB >> e.stopPropagationCB

    def onItemDragOver(P: NodeProps)(e: ReactEventH): Callback =
      P.onNodeDrag($.asInstanceOf[NodeC]) >> e.preventDefaultCB >> e.stopPropagationCB

    def childrenFromProps(P: NodeProps): CallbackTo[Option[Unit]] =
      $.modState(S => S.copy(children = if (S.children.isEmpty) P.root.children else Nil))
        .conditionally(P.root.children.nonEmpty)


    def isFilterTextExist(filterText: String, data: TreeItem): Boolean = {
      def matches(treeItem: TreeItem): Boolean ={
        treeItem.item match {
          case relation: Relation => relation.entity.toString.toLowerCase.contains(filterText.toLowerCase)
          case node: Node => node.toString.toLowerCase.contains(filterText.toLowerCase)
        }
      }

      def loop(data: Seq[TreeItem]): Boolean =
        data.view.exists(
          item => matches(item) || loop(item.children)
        )

      matches(data) || loop(data.children)
    }

    def onDrop(P: NodeProps)(event: ReactDragEvent): Callback = {
      event.preventDefault()

      val pathFrom = event.dataTransfer.getData("path").split("/")
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

        if (event.dataTransfer.getData("existing") == "false")
          dispatch(AddElem(pathTo, getElem(event)))
        else if (isAttribute || pathFrom.diff(pathTo).isEmpty)  // Hindra att dra till Attribute och sina egna children
          dispatch(NoAction)
        else if(!pathFrom.init.corresponds(pathTo)(_ == _ ))
          dispatch(MoveElem(pathFrom, pathTo)) >> dispatch(RemoveElem(pathFrom))
        else
          dispatch(NoAction)
    }


    def dragOver(P:NodeProps)(e: ReactDragEvent): Callback = {
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
        <.div(
          ^.overflow.hidden,
          ^.position.relative,
          ^.border := "1px solid",
          ^.borderRadius := "5px",
          ^.backgroundColor := "#e3e8ea",
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
          ^.onDragOver ==> onItemDragOver(P),
          S.selected ?= P.style.selectedTreeItemContent,
          S.draggedOver ?= P.style.dragOverTreeItemContent,
          <.span(
            ^.id := P.root.item.toString,
            ^.unselectable := "true",
            //toText(P.root),
            P.root.item.toString + getRelationType(P.root).getOrElse(""),
            ^.position := "absolute",
            ^.left := "7%",
            ^.top := "25%",
            ^.fontSize := "large"

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

  case class NodeState(children: Seq[TreeItem] = Nil, selected: Boolean = false, draggedOver: Boolean = false)

  case class NodeProps(root: TreeItem,
                       open: Boolean,
                       depth: Int = 0,
                       parent: String = "",
                       onNodeSelect: (NodeC) => Callback,
                       onNodeDrag: (NodeC) => Callback,
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
    .initialState(State("", filterMode = false, js.undefined, js.undefined))
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