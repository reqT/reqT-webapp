package example

import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.CompScope._
import japgolly.scalajs.react.{Callback, _}
import japgolly.scalajs.react.vdom.prefix_<^.{^, _}

import scalacss.ScalaCssReact._
import scala.scalajs.js
import shared.{Elem, _}
import diode.NoAction
import modals.{AddElemModal, DeleteModal, EditModal}
import org.scalajs.dom
import org.scalajs.dom._
import selects.RelationSelect

case class TreeItem(var item: Any, var uuid: UUID, var children: Seq[TreeItem], var link: Option[RelationType]) {

  def linkToString: String = link match {
    case Some(relationType) => relationType.getType
    case None => ""
  }

  def itemToString: String = item.toString.replaceAll("\"", "")

  def entityToString: String =
    item match {
      case i: Entity => i.getType
      case i: IntAttribute => i.getType
      case i: StringAttribute => i.getType
      case "Model" => item.toString
    }

  def contentToString: String =
    item match {
      case i: Entity => i.getID
      case i: IntAttribute => i.getValue.toString
      case i: StringAttribute => i.getValue
      case "Model" => item.toString
    }

  def apply(item: Any, uuid: UUID, children: Seq[TreeItem]): TreeItem = this (item, uuid, Seq())

}

object ReactTreeView {

  case class Tuple(var uuid: UUID, collapsed: Boolean)
  case class AddTuple(tuple: Tuple) extends Action
  case class RemoveTuple(tuple: Tuple) extends Action
  case class ToggleCollapsed(uuid: UUID) extends Action

  trait Style {
    def reactTreeView = Seq[TagMod]()

    def treeGroup = Seq(^.padding := "0 0 0 40px")

    def treeItem = Seq(
      ^.listStyleType := "none"
      //      ^.marginTop := "1px",
//      ^.paddingLeft := "20px"
    )

    def treeItemDiv = Seq(
      ^.boxShadow := "5px 6px 12px 0px rgba(0,0,0,0.2)",
      ^.overflow.visible,
      ^.position.relative,
      ^.borderRadius := "5px",
      ^.padding := "5px",
      ^.marginBottom := "18px",
      ^.marginRight := "0",
      ^.marginLeft := "0",
      ^.width := "500px",
      ^.key := "toggle",
      ^.className := "container",
      ^.draggable := true
    )

    def treeItemIdDiv = Seq(
      ^.className := "row",
      ^.overflow.hidden,
      ^.unselectable := "true",
      ^.position := "absolute",
      ^.left := "7%",
      ^.top := "0px",
      ^.bottom := "0px",
      ^.width := "440px",
      ^.paddingLeft := "0px",
      ^.paddingRight := "0px",
      ^.fontSize := "medium"
    )

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

    def attributeDiv = Seq(
      ^.className := "col",
      ^.height := "100%",
      ^.width := "40%",
      ^.top := "0px",
      ^.bottom := "0px",
      ^.position := "absolute",
      ^.left := "0%",
      ^.paddingTop := "3%",
      ^.paddingLeft := "3%"
    )

    def elemDiv1 = Seq(
      ^.fontStyle.oblique,
      ^.className := "col",
      ^.height := "100%",
      ^.width := "30%",
      ^.top := "0px",
      ^.bottom := "0px",
      ^.position := "absolute",
      ^.left := "0%",
      ^.paddingTop := "3%",
      ^.paddingLeft := "3%"
    )

    def elemDiv2 = Seq(
      ^.width := "0px",
      ^.height := "100%",
      ^.float.left,
      ^.border := "1px inset",
      ^.left := "30%",
      ^.position := "absolute",
      ^.top := "0px",
      ^.bottom := "0px",
      ^.opacity := "0.5"
    )

    def elemDiv3 = Seq(
      ^.className := "col",
      ^.height := "100%",
      ^.width := "70%",
      ^.top := "0px",
      ^.bottom := "0px",
      ^.paddingLeft := "3%",
      ^.position := "absolute",
      ^.left := "30%",
      ^.overflow.hidden,
      ^.textAlign.justify,
      ^.fontSize.small
    )

    def treeItemHasChildrenClosed = Seq(^.contentStyle := "▶")

    def treeItemHasChildrenOpened = Seq(^.contentStyle := "▼")
  }

  def getRelationType(treeItem: TreeItem): Option[String] = {
    treeItem.item match {
      case "Model" => None
      case elem: Elem => if (elem.hasRelation) Some(treeItem.linkToString) else None
    }
  }

  type NodeC = DuringCallbackU[NodeProps, NodeState, NodeBackend]

  case class State(filterText: String,
                   filterMode: Boolean,
                   selectedNode: js.UndefOr[NodeC],
                   dragOverNode: js.UndefOr[NodeC],
                   scrollPosition: Double,
                   collapseList: Seq[Tuple] = Seq[Tuple](),
                   openModals: OpenModals = OpenModals(),
                   modelProps: ModelProps = ModelProps()
                  )

  case class Props(root: TreeItem,
                   open: Boolean,
                   onItemSelect: js.UndefOr[(String, String, Int) => Callback],
                   showSearchBox: Boolean,
                   style: Style,
                   modelProxy: ModelProxy[Tree],
                   saveScrollPosition: Double => Callback
                  )

  case class ModelProps(treeItem: TreeItem = null, dispatch: (Action => Callback) = null, path: Seq[String] = Seq(),
                        elemToAdd: Option[Elem] = None, addToPlaceholder: Boolean = false)

  case class OpenModals(isAddElemModalOpen: Boolean = false, isEditModalOpen: Boolean = false, isDeleteModalOpen: Boolean = false)


  class Backend($: BackendScope[Props, State]) {

    def closeDeleteModal: Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isDeleteModalOpen = false)))

    def closeEditModal: Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isEditModalOpen = false)))

    def closeAddElemModal: Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isAddElemModalOpen = false)))

    def openDeleteModal(newTreeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String]) = $.modState(S =>
      S.copy(openModals = S.openModals.copy(isDeleteModalOpen = true),
        modelProps = ModelProps(newTreeItem, newDispatch, newPath, None)))

    def openEditModal(newTreeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String]) = $.modState(S =>
      S.copy(openModals = S.openModals.copy(isEditModalOpen = true),
        modelProps = ModelProps(newTreeItem, newDispatch, newPath, None)))

    def openAddElemModal(newTreeItem: TreeItem, newDispatch: Action => Callback, newPath: Seq[String], newElem: Option[Elem], newAddToPlaceHolder: Boolean) = $.modState(S =>
      S.copy(openModals = S.openModals.copy(isAddElemModalOpen = true),
        modelProps = ModelProps(treeItem = newTreeItem, path = newPath, dispatch = newDispatch, elemToAdd = newElem, addToPlaceholder = newAddToPlaceHolder)))

    def saveScrollPosition(position: Double): Callback = $.modState(s => s.copy(scrollPosition = position))

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

      removeSelection >> updateThis >> setSelection
    }

    def onNodeEnter(P: Props)(draggedOver: NodeC): Callback = {

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
        draggedOver.modState(S => S.copy(draggedOver = true, dragEnter = S.dragEnter + 1))

      val shouldUpdate: Callback = draggedOver.modState(_.copy(shouldUpdate = true))

      shouldUpdate.runNow()
      removeCurrent >> updateThis >> setCurrent
    }

    def onNodeLeave(P: Props)(draggedOver: NodeC): Callback = {

      val removeCurrent: Callback =
        $.state.flatMap(
          _.dragOverNode
            .filterNot(_ == draggedOver)
            .filter(_.isMounted())
            .fold(Callback.empty)(_.modState(_.copy(draggedOver = false)))
        )

      val setRemove: Callback =
        draggedOver.modState(_.copy(draggedOver = false))

      val shouldUpdate: Callback = draggedOver.modState(_.copy(shouldUpdate = true))

      shouldUpdate.runNow()
      removeCurrent >> setRemove
    }

    def onTextChange(text: String): Callback =
      $.modState(_.copy(filterText = text, filterMode = true))

    def render(P: Props, S: State) =
      <.div(
        ^.width := "100%",
        ^.height := "100%",
        DeleteModal(S.openModals.isDeleteModalOpen, closeDeleteModal, S.modelProps.treeItem, S.modelProps.dispatch, S.modelProps.path),
        EditModal(S.openModals.isEditModalOpen, closeEditModal, S.modelProps.treeItem, S.modelProps.dispatch, S.modelProps.path),
        AddElemModal(S.openModals.isAddElemModalOpen, closeAddElemModal, S.modelProps.treeItem, S.modelProps.dispatch, S.modelProps.path, S.modelProps.elemToAdd, S.modelProps.addToPlaceholder),
        P.style.reactTreeView)(
          <.div(
            P.showSearchBox ?= ReactSearchBox(onTextChange = onTextChange)
          ),
          cc(proxy => TreeNode.withKey("root")(
            NodeProps(
              root = P.root,
              open = P.open,
              onNodeSelect = onNodeSelect(P),
              onNodeEnter = onNodeEnter(P),
              onNodeLeave = onNodeLeave(P),
              filterText = S.filterText,
              style = P.style,
              filterMode = S.filterMode,
              modelProxy = P.modelProxy,
              openDeleteModal = openDeleteModal,
              openEditModal = openEditModal,
              openAddElemModal = openAddElemModal,
              saveScrollPosition = P.saveScrollPosition,
              collapseProxy = proxy
            )
          )
          )
      )
  }
  var zoomFactor: Double = 1
  def zoomIn: Callback = {
    if(zoomFactor < 1)
      Callback({
        zoomFactor = zoomFactor + 0.05
        val tree = document.getElementById("ReactTreeView").asInstanceOf[dom.html.LI]
        var styleList = tree.getAttribute("style").split(";")
        styleList = styleList.filter(p => !p.contains("transform"))
        tree.setAttribute("style", (s"transform: scale($zoomFactor)" +: styleList).mkString(";"))
        }
      )
    else
      Callback()
  }

  def zoomOut: Callback = {
    if(zoomFactor > 0)
      Callback({
        zoomFactor = zoomFactor - 0.05
        val tree = document.getElementById("ReactTreeView").asInstanceOf[dom.html.LI]
        var styleList = tree.getAttribute("style").split(";")
        styleList = styleList.filter(p => !p.contains("transform"))
        tree.setAttribute("style", (s"transform: scale($zoomFactor)" +: styleList).mkString(";"))
      }
      )
    else
      Callback()
  }

  case class NodeBackend($: BackendScope[NodeProps, NodeState]) {

    import upickle.default.read

    def onDragStart(P: NodeProps)(event: ReactDragEvent): Callback = {
      val path = if (P.parent.isEmpty) P.root.uuid.toString
      else P.parent + "/" + P.root.uuid.toString

      event.dataTransfer.effectAllowed = "move"
      event.dataTransfer.setData("existing", "true")
      event.dataTransfer.setData("path", path)

      $.modState(_.copy(dragEnter = 0))
    }

    def onTreeMenuToggle(P: NodeProps)(e: ReactEventH): Callback =
      P.collapseProxy.dispatchCB(ToggleCollapsed(P.root.uuid)) >> P.onNodeSelect($.asInstanceOf[NodeC]) >> childrenFromProps(P) >> e.preventDefaultCB >> e.stopPropagationCB

//    def onItemSelect(P: NodeProps)(e: ReactEventH): Callback =
//      P.onNodeSelect($.asInstanceOf[NodeC]) >> e.preventDefaultCB >> e.stopPropagationCB

    def onDragOver(P: NodeProps)(e: ReactEventH): Callback =
      e.preventDefaultCB >> e.stopPropagationCB

    def onDragEnter(P: NodeProps)(e: ReactDragEvent): Callback =
      contract >> P.onNodeEnter($.asInstanceOf[NodeC]) >> e.preventDefaultCB >> e.stopPropagationCB >> Callback(println("EnterNode"))

    def onDragEnd(P: NodeProps)(e: ReactDragEvent): Callback =
      P.onNodeLeave($.asInstanceOf[NodeC]) >> $.modState(_.copy(shouldUpdate = false, showTemp1 = false, showTemp2 = false)) >> e.preventDefaultCB >> e.stopPropagationCB

    def onDragLeave(P: NodeProps)(e: ReactDragEvent): Callback =
      P.onNodeLeave($.asInstanceOf[NodeC]) >> e.preventDefaultCB >> e.stopPropagationCB >> $.modState(_.copy(shouldUpdate = false)) >> Callback(println("LeaveNode"))

    def onDragEnterLI(P: NodeProps)(e: ReactDragEvent): Callback = {
      $.modState(_.copy(shouldUpdate = true)).runNow()
      expand >> e.preventDefaultCB >> e.stopPropagationCB >> Callback(println("EnterLI"))
    }

    def onDragLeaveLI(P: NodeProps)(e: ReactDragEvent): Callback = {
      $.modState(_.copy(shouldUpdate = true)).runNow()
      contract >> e.preventDefaultCB >> e.stopPropagationCB >> Callback(println("LeaveLI"))
    }


    def onDragEnterUL(P: NodeProps)(e: ReactDragEvent): Callback = {
      $.modState(_.copy(shouldUpdate = true)).runNow()
      expand2 >> e.preventDefaultCB >> e.stopPropagationCB >> Callback(println("EnterUL"))
    }

    def onDragLeaveUL(P: NodeProps)(e: ReactDragEvent): Callback =
      contract2 >> e.preventDefaultCB >> e.stopPropagationCB >> Callback(println("LeaveUL"))

    def expand2: Callback = $.modState(_.copy(showTemp2 = true))
    def contract2: Callback = $.modState(_.copy(showTemp2 = false))

    def expand: Callback = $.modState(S => S.copy(showTemp1 = true, dragEnter = S.dragEnter + 1))

    def contract: Callback =
    {
      $.modState(S => {
        if (S.dragEnter <= 1)
          S.copy(showTemp1 = false, dragEnter = S.dragEnter - 1)
        else
          S.copy(dragEnter = S.dragEnter - 1)
      })
    }

    def childrenFromProps(P: NodeProps): CallbackTo[Option[Unit]] ={
      $.modState(S => S.copy(children = if (S.children.isEmpty){
        P.root.children
      }
      else Nil))
        .when(P.root.children.nonEmpty)
    }


    def matchesFilterText(filterText: String, data: TreeItem): Boolean = {
      def trim(data: String): String = data.toString.replaceAll("""(UUID\([^\)]*\))|TreeItem|List|None|[\(\),"]|Some""", " ").trim.replaceAll(" +", " ")

      def matchesType(treeItem: TreeItem): Boolean =
        treeItem.item match {
          case _: Entity => filterText.toLowerCase.contains("entity") || filterText.toLowerCase.contains("entities")
          case _: IntAttribute => filterText.toLowerCase.contains("attribute") && !filterText.toLowerCase.contains("stringattribute")
          case _: StringAttribute => filterText.toLowerCase.contains("attribute") && !filterText.toLowerCase.contains("intattribute")
          case _ => false
        }

      def matches(treeItem: TreeItem): Boolean = trim(treeItem.toString).toLowerCase.contains(filterText.toLowerCase)

      def relationTypeMatches(treeItem: TreeItem): Boolean = trim(treeItem.linkToString).toLowerCase.contains(filterText.toLowerCase)

      def loop(data: Seq[TreeItem]): Boolean =
        data.view.exists(
          item => matches(item) || loop(item.children) || relationTypeMatches(item) || matchesType(item)
        )

      (matches(data) || loop(data.children) || relationTypeMatches(data) || matchesType(data)) && (data.item != "Model")
    }

    def onDrop(P: NodeProps)(event: ReactDragEvent): Callback = {

      val pathFrom = event.dataTransfer.getData("path").split("/")
      val pathTo = (if (P.parent.isEmpty) P.root.uuid.toString
      else P.parent + "/" + P.root.uuid).split("/")

      val dispatch: Action => Callback = P.modelProxy.dispatchCB

      val ctrlHeld = event.ctrlKey
      val notExistingElem = event.dataTransfer.getData("existing") == "false"
      val dropOnSelf = pathFrom.diff(pathTo).isEmpty
      val dropOnItsChildren = pathFrom.init.corresponds(pathTo)(_ == _)
      var isAttribute = false

      val collapsed = P.collapseProxy.value.find(_.uuid == P.root.uuid) match {
        case Some(tuple) =>
          tuple.collapsed
        case None =>
          AddTuple(Tuple(P.root.uuid, collapsed = false))
          false
      }

      if(collapsed) {
        P.collapseProxy.dispatchCB(ToggleCollapsed(P.root.uuid)).runNow()
      }

      if (P.root.item.toString != "Model")
        isAttribute = P.root.item.asInstanceOf[Elem].isAttribute

      def getElem(event: ReactDragEvent): Elem = event.dataTransfer.getData("type") match {
        case "entity" => read[Entity](event.dataTransfer.getData("elem"))
        case "stringAttr" => read[StringAttribute](event.dataTransfer.getData("elem"))
        case "intAttr" => read[IntAttribute](event.dataTransfer.getData("elem"))
      }

      def shouldUpdate = $.modState(_.copy(shouldUpdate = true))
      def setNotDraggedOver = $.modState(_.copy(draggedOver = false))
      def closeTemp = $.modState(_.copy(showTemp1 = false, showTemp2 = false))


      def action = {
        if (ctrlHeld)
          dispatch(CopyElem(pathFrom, pathTo, RelationType("has")))
        else if (isAttribute || dropOnSelf || dropOnItsChildren) {
          dispatch(NoAction)
        } else if (notExistingElem) {
          getElem(event) match {
            case entity: Entity => P.openAddElemModal(P.root, dispatch, pathTo, Some(entity), false)
            case intAttr: IntAttribute => P.openAddElemModal(P.root, dispatch, pathTo, Some(intAttr), false)
            case stringAttr: StringAttribute => P.openAddElemModal(P.root, dispatch, pathTo, Some(stringAttr), false)
            case _ => Callback()
          }
        } else {
          dispatch(MoveElem(pathFrom, pathTo, RelationType("has"))) >> dispatch(RemoveElem(pathFrom)) >> dispatch(RemoveEmptyRelation(pathFrom.init))
        }
      }
      shouldUpdate.runNow()
      event.stopPropagationCB >> action >> setNotDraggedOver >> closeTemp
    }

    def onDoubleClickTreeItem(P: NodeProps, S: NodeState)(e: ReactEvent): Callback = {
      val dispatch: Action => Callback = P.modelProxy.dispatchCB
      val path = (if (P.parent.isEmpty) P.root.uuid.toString else P.parent + "/" + P.root.uuid).split("/")

      P.root.item match {
        case _: Elem => P.openEditModal(P.root, dispatch, path)
        case _ => Callback()
      }
    }

    def removeElem(P: NodeProps)(e: ReactEventI): Callback = {
      val path = if (P.parent.isEmpty) P.root.uuid.toString else P.parent + "/" + P.root.uuid
      val dispatch: Action => Callback = P.modelProxy.dispatchCB


      $.modState(_.copy(shouldUpdate = true)).runNow()
      P.openDeleteModal(P.root, dispatch, path.split("/")) >> e.stopPropagationCB >> e.preventDefaultCB
    }

    def dragOverStyle(P: NodeProps): Seq[TagMod] = {
      Seq(
        ^.border := "1px solid black",
        if(P.root.item.isInstanceOf[Attribute]) ^.borderColor := "red" else ""
      )
    }

    def placeholderStyle(P: NodeProps): Seq[TagMod] = {
      Seq(^.opacity := 0.5,
        ^.textAlign := "center",
        ^.marginTop := "15px",
        ^.marginBottom :="15px",
        ^.verticalAlign := "center",
        ^.width := "500px",
        ^.borderRadius := "5px",
        ^.height := "50px",
        ^.border := "2px dashed",
        ^.borderColor := "black",
        ^.transform := {
          if (P.root.children.nonEmpty) "translate(47px,0px)" else "none"
        }
      )
    }

    def setContentDivSize(content: String): Seq[TagMod] = {
      var cont: String = content
      val nbrInserts = content.length / 37

      for (i <- 1 to nbrInserts) {
        if (i == 1) {
          cont = cont.substring(0, i * 37) + "\n" + cont.substring(i * 37, cont.length)
        } else {
          cont = cont.substring(0, i * 36) + "...\n\n" + cont.substring(i * 36, cont.length)
        }
      }
      Seq(
        cont,
        ^.whiteSpace := "pre-line"
      )
    }

    def dropOnPlaceholder(P: NodeProps, dropAfterChildren: Boolean)(event: ReactDragEvent): Callback = {
      val pathFrom = event.dataTransfer.getData("path").split("/")
      val pathTo = (if (P.parent.isEmpty) P.root.uuid.toString
      else P.parent + "/" + P.root.uuid).split("/")

      val dispatch: Action => Callback = P.modelProxy.dispatchCB

      val ctrlHeld = event.ctrlKey
      val notExistingElem = event.dataTransfer.getData("existing") == "false"
      val dropOnSelf = pathFrom.diff(pathTo).isEmpty

      def getElem(event: ReactDragEvent): Elem = event.dataTransfer.getData("type") match {
        case "entity" => read[Entity](event.dataTransfer.getData("elem"))
        case "stringAttr" => read[StringAttribute](event.dataTransfer.getData("elem"))
        case "intAttr" => read[IntAttribute](event.dataTransfer.getData("elem"))
      }

      def shouldUpdate = $.modState(_.copy(shouldUpdate = true))
      def closeTemp = $.modState(_.copy(showTemp1 = false, showTemp2 = false))

      def getAction: Callback = {
        if (ctrlHeld)
          dispatch(CopyElemToPlaceholder(pathFrom, pathTo, dropAfterChildren))
        else if (dropOnSelf) {
          dispatch(NoAction)
        } else if (notExistingElem) {
          getElem(event) match {
            case entity: Entity => P.openAddElemModal(P.root, dispatch, pathTo, Some(entity), true)
            case intAttr: IntAttribute => P.openAddElemModal(P.root, dispatch, pathTo, Some(intAttr), true)
            case stringAttr: StringAttribute => P.openAddElemModal(P.root, dispatch, pathTo, Some(stringAttr), true)
            case _ => Callback()
          }
        } else
          dispatch(MoveElemToPlaceholder(pathFrom, pathTo, dropAfterChildren)) >> dispatch(RemoveElem(pathFrom)) >> dispatch(RemoveEmptyRelation(pathFrom.init))
      }
      shouldUpdate.runNow()
      event.stopPropagationCB >> getAction >> closeTemp
    }

    def render(P: NodeProps, S: NodeState): ReactTag = {
      val dispatch: Action => Callback = P.modelProxy.dispatchCB
      val depth = P.depth + 1

      val parent = if (P.parent.isEmpty) P.root.uuid.toString
      else s"${P.parent}/${P.root.uuid.toString}"

      val path = (if (P.parent.isEmpty) P.root.uuid.toString
      else P.parent + "/" + P.root.uuid).split("/")

      val updateRel = UpdateRelation(path, P.root.item.asInstanceOf[Entity].id, _: Option[RelationType])

      val collapsed = P.collapseProxy.value.find(_.uuid == P.root.uuid) match {
        case Some(tuple) =>
          tuple.collapsed
        case None =>
          P.collapseProxy.dispatchCB(AddTuple(Tuple(P.root.uuid, collapsed = false)))
          false
      }

      val treeMenuToggle: TagMod =

        if (S.children.nonEmpty && !collapsed)
        <.span(
          ^.onClick ==> onTreeMenuToggle(P),
          ^.onDblClick ==> onTreeMenuToggle(P),
          ^.key := "arrow",
          P.style.treeItemBefore,
          "▼"
        )
        else if (P.root.children.nonEmpty && S.children.isEmpty)
          <.span(
            ^.onClick ==> onTreeMenuToggle(P),
            ^.onDblClick ==> onTreeMenuToggle(P),
            ^.key := "arrow",
            P.style.treeItemBefore,
            "▶"
          )
        else ""

      <.div(
        ^.width := "100%",
        ^.height := "100%",
        ^.onDragOver ==> onDragOver(P),
        ^.onDrop ==> onDrop(P),
        <.li(
          ^.zIndex := "-1",
          ^.onDragEnter ==> onDragEnterLI(P),
          ^.onDragLeave ==> onDragLeaveLI(P),
          ^.onDragOver ==> onDragOver(P),
          ^.onDrop ==> dropOnPlaceholder(P, dropAfterChildren = false),
//          ^.transform := "translateX(6.5px)",
          ^.border := "1px solid rgba(211,211,211,0.5)",
          ^.borderTop := "1px solid rgba(211,211,211,0.5)",
          P.style.treeItem,
          <.div(
            P.style.treeItemDiv,
            ^.borderBottomRightRadius := {
              if (P.root.children.isEmpty) "5px" else "0px"
            },
            ^.borderTopRightRadius := {
              if (P.root.children.isEmpty) "5px" else "0px"
            },
            ^.backgroundColor := {
              if (P.root.item.isInstanceOf[Entity]) "#CEDBE7"
              else "#CFEADD"
            },
            treeMenuToggle,
            ^.onDrop ==> onDrop(P),
            ^.onDragStart ==> onDragStart(P),
            ^.onDragEnd ==> onDragEnd(P),
            ^.onDragLeave ==> onDragLeave(P),  //Denna förstör
            ^.onDragOver ==> onDragOver(P),
            ^.onDragEnter ==> onDragEnter(P),
            ^.onDblClick ==> onDoubleClickTreeItem(P, S),
            S.draggedOver ?= dragOverStyle(P),
            <.div(
              ^.pointerEvents := "none",
              P.style.treeItemIdDiv,
              ^.id := P.root.itemToString,
              if (P.root.item.isInstanceOf[Elem]) {
                Seq(
                  <.div(
                    ^.pointerEvents := "none",
                    P.style.elemDiv1,
                    ^.fontSize := {
                      if (P.root.entityToString.length > 12) "small" else "medium"
                    },
                    P.root.entityToString
                  ),
                  <.div(
                    ^.pointerEvents := "none",
                    P.style.elemDiv2
                  ),
                  <.div(
                    ^.pointerEvents := "none",
                    P.style.elemDiv3,
                    ^.paddingTop := { if(P.root.contentToString.length >= 38 || P.root.contentToString.contains("\n")) "1.8%" else "3%" },
                    setContentDivSize(P.root.contentToString)
                  )
                )
              } else {
                <.div(
                  ^.pointerEvents := "none",
                  P.style.attributeDiv,
                  <.span(
                    ^.pointerEvents := "none",
                    P.root.entityToString
                  )
                )
              }
            ),
            <.button(
              Styles.bootStrapRemoveButton,
              ^.onClick ==> removeElem(P)
            ),
            getRelationType(P.root) match {
              case Some(relation) =>
                <.div(
                  S.draggedOver ?= dragOverStyle(P),
                  ^.position.absolute,
                  ^.top := "0%",
                  ^.height := "100%",
                  ^.left := "100%",
                  RelationSelect(relation, Some(dispatch), Some(updateRel), isModelValue = true, None, Some(P.saveScrollPosition(_)))
                )
              case None =>
                <.div()
            }
          ),
          <.div(
            ^.pointerEvents := "none",
            ^.zIndex := "-5",
            S.showTemp1 ?= placeholderStyle(P)
          ),
          if (S.children.nonEmpty){
          <.ul(P.style.treeGroup)(
            ^.marginBottom := "0px",
//            ^.border := "1px dashed green",
            ^.onDragEnter ==> onDragEnterUL(P),
            ^.onDragLeave ==> onDragLeaveUL(P),
            ^.onDragOver ==> onDragOver(P),
            ^.onDrop ==> dropOnPlaceholder(P, dropAfterChildren = true),
            S.children.map(child =>
              (matchesFilterText(P.filterText, child) || matchesFilterText(P.filterText, P.root)) ?=
                cc(proxy => TreeNode.withKey(s"$parent/${child.uuid}")(P.copy(
                  root = child,
                  open = P.open,
                  depth = depth,
                  parent = parent,
                  filterText = P.filterText,
                  collapseProxy = proxy
                ))
                )
            ))}else {""}
        ),
        if (P.root.children.nonEmpty) {
          <.div(
            ^.pointerEvents := "none",
            S.showTemp2 ?= placeholderStyle(P),
            ^.transform := "translateX(6px)"
          )
        } else {
          <.div(^.pointerEvents := "none")
        }
      )
    }
  }


  case class NodeState(children: Seq[TreeItem], selected: Boolean = false, draggedOver: Boolean = false, scrollPosition: Double = 0,
                       showTemp1: Boolean = false, showTemp2: Boolean = false, shouldUpdate: Boolean = false, dragEnter: Int = 0)

  val cc = CollapseCircuit.connect(_.list)


  case class NodeProps(root: TreeItem,
                       open: Boolean,
                       depth: Int = 0,
                       parent: String = "",
                       onNodeSelect: (NodeC) => Callback,
                       onNodeEnter: (NodeC) => Callback,
                       onNodeLeave: (NodeC) => Callback,
                       filterText: String,
                       style: Style,
                       filterMode: Boolean,
                       modelProxy: ModelProxy[Tree],
                       openDeleteModal: (TreeItem, (Action => Callback),  Seq[String]) => Callback,
                       openEditModal: (TreeItem, (Action => Callback),  Seq[String]) => Callback,
                       openAddElemModal: (TreeItem, Action => Callback ,  Seq[String], Option[Elem], Boolean) => Callback,
                       saveScrollPosition: Double => Callback,
                       collapseProxy: ModelProxy[Seq[Tuple]]
                      )


  lazy val TreeNode = ReactComponentB[NodeProps]("ReactTreeNode")
    .initialState_P(P => NodeState(P.root.children))
    .renderBackend[NodeBackend]
      .componentWillMount(x =>
        x.props.collapseProxy.dispatchCB(AddTuple(Tuple(x.props.root.uuid, collapsed = false))) >>
        {
          x.props.collapseProxy.value.find(_.uuid == x.props.root.uuid) match {
            case Some(tuple) =>
              if(!tuple.collapsed)
                x.modState(_.copy(children = x.props.root.children))
              else
                x.modState(_.copy(children = Nil))
            case None =>
              x.modState(_.copy(children = x.props.root.children))
          }
        }
      )
    .componentWillReceiveProps {
      case ComponentWillReceiveProps(_$, newProps) =>
        _$.modState(_.copy(children = if (newProps.open) newProps.root.children else Nil))
          .when(newProps.filterMode)
          .void
    }
    .shouldComponentUpdate(x => if (x.nextState.selected || x.nextState.shouldUpdate || x.currentState.draggedOver || x.currentState.shouldUpdate) true else false)
//      .componentWillUpdate(x => {
//        x.$.props.saveScrollPosition(document.getElementById("treeView").scrollTop)
//      })
    .build

  val component = ReactComponentB[Props]("ReactTreeView")
    .initialState(State("", filterMode = false, js.undefined, js.undefined, scrollPosition = 0))
    .renderBackend[Backend]
    .componentWillUpdate(x => {
        x.$.props.saveScrollPosition(document.getElementById("treeView").scrollTop) >>
        Callback(x.nextState.copy(collapseList =  x.nextState.collapseList ++ x.currentState.collapseList))
      })
    .build


  def apply(root: TreeItem,
            openByDefault: Boolean = true,
            onItemSelect: js.UndefOr[(String, String, Int) => Callback] = js.undefined,
            showSearchBox: Boolean = false,
            ref: js.UndefOr[String] = js.undefined,
            key: js.UndefOr[js.Any] = js.undefined,
            style: Style = new Style {},
            modelProxy: ModelProxy[Tree],
            saveScrollPosition: Double => Callback

           ) =
    component.set(key, ref)(Props(root, openByDefault, onItemSelect, showSearchBox, style, modelProxy, saveScrollPosition))

}