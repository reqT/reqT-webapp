//package example
//
//import diode.Action
//import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
//import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent}
//import org.scalajs.dom.ext.KeyCode
//import japgolly.scalajs.react._
//import shared._
//
//
//object Modal {
//
//  sealed trait ModalType
//
//  case object EMPTY_MODAL extends ModalType
//
//  case object ADD_ELEM_MODAL extends ModalType
//
//  case object EDIT_MODAL extends ModalType
//
//  case object DELETE_MODAL extends ModalType
//
//  def modalStyle = Seq(
//    ^.width := "400px",
//    ^.padding := "5px",
//    ^.position := "absolute",
//    ^.border := "1px solid #CCC",
//    ^.borderRadius := "5px",
//    ^.top := "40%",
//    ^.left := "50%",
//    ^.transform := "translate(-50%,-50%)",
//    ^.zIndex := "9999",
//    ^.background := "#FFF",
//    ^.paddingBottom := "15px",
//    ^.paddingRight := "15px",
//    ^.paddingTop := "15px",
//    ^.paddingLeft := "15px",
//    ^.boxShadow := "rgba(0, 0, 0, 0.2) 5px 6px 12px 0px"
//  )
//
//  def backdropStyle = Seq(
//    ^.position := "absolute",
//    ^.width := "100%",
//    ^.height := "100%",
//    ^.top := "0px",
//    ^.left := "0px",
//    ^.zIndex := "9998",
//    ^.background := "#CCC",
//    ^.opacity := "0.5"
//  )
//
//
//  case class State(input: String, newEntity: Option[Entity], newRelation: Option[RelationType], newAttribute: Option[Attribute])
//
//  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback, modalProps: webApp.ModelProps)
//
//  class Backend($: BackendScope[Props, State]) {
//    def render(P: Props, S: State) =
//      if (P.isOpen) {
//        <.div(
//          ^.onKeyDown ==> handleKeyDown(P, S),
//          <.div(
//            modalStyle,
//            getModalStyle(P, S)
//          ),
//          <.div(
//            backdropStyle,
//            ^.onClick ==> onClose(P)
//          )
//        )
//      } else
//        <.div()
//
//    def resetState: Callback = $.modState(_.copy(input = "", newEntity = None, newRelation = None, newAttribute = None))
//
//    def onClose(P: Props)(e: ReactEvent): Callback = P.onClose(e) >> resetState
//
//    def onSave(P: Props, S: State)(e: ReactEvent): Callback = {
//      P.modalProps.treeItem.item match {
//        case entity: Entity => if (entity.hasRelation) {
//          S.newEntity.get.setID(S.input)
//          S.newEntity.get.hasRelation = true
//          P.modalProps.dispatch(UpdateEntireRelation(path = P.modalProps.path, newEntity = S.newEntity.get, S.newRelation)) >> onClose(P)(e)
//        } else {
//          S.newEntity.get.setID(S.input)
//          P.modalProps.dispatch(UpdateEntity(path = P.modalProps.path, newEntity = S.newEntity.get)) >> onClose(P)(e)
//        }
//
//        case _: IntAttribute =>
//          P.modalProps.dispatch(UpdateIntAttribute(path = P.modalProps.path, S.newAttribute.getOrElse(P.modalProps.treeItem.item).asInstanceOf[IntAttribute].setValue(S.input.toInt))) >> onClose(P)(e)
//        case _: StringAttribute =>
//          P.modalProps.dispatch(UpdateStringAttribute(path = P.modalProps.path, S.newAttribute.getOrElse(P.modalProps.treeItem.item).asInstanceOf[StringAttribute].setValue(S.input))) >> onClose(P)(e)
//
//      }
//    }
//
//    def inputChanged(e: ReactEventI): Callback = {
//      val newInput = e.target.value
//      $.modState(_.copy(input = newInput))
//    }
//
//    def intInputChanged(e: ReactEventI): Callback = {
//      val newInput = e.target.value
//      $.modState(_.copy(input = newInput.replaceAll("[^\\d]", "")))
//    }
//
//    def getModalStyle(P: Props, S: State): Seq[TagMod] = {
//      P.modalProps.modalType match {
//        case EDIT_MODAL => editModalStyle(P, S)
//        case ADD_ELEM_MODAL => addElemModalStyle(P, S)
//        case DELETE_MODAL => deleteElemModalStyle(P, S)
//        case EMPTY_MODAL => Seq("Error 404")
//      }
//    }
//
//    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventI): Callback = {
//      if (e.nativeEvent.keyCode == KeyCode.Escape) {
//        onClose(P)(e)
//      } else if (e.nativeEvent.keyCode == KeyCode.Enter && !e.shiftKey) {
//        onEnter(P, S)(e)
//      } else {
//        Callback()
//      }
//    }
//
//    def onEnter(P: Props, S: State)(e: ReactEvent): Callback = {
//      P.modalProps.modalType match {
//        case EDIT_MODAL => onSave(P, S)(e)
//        case ADD_ELEM_MODAL => addElem(P, S)(e)
//        case DELETE_MODAL => onDelete(P)(e)
//        case EMPTY_MODAL => onClose(P)(e)
//      }
//    }
//
//    def addElem(P: Props, S: State)(e: ReactEvent): Callback = P.modalProps.dispatch(AddElem(P.modalProps.path, prepareElem(S.input, P.modalProps.elemToAdd), RelationType("has"))) >> onClose(P)(e)
//
//    def prepareElem(newValue: String, elem: Option[Elem]): Elem = {
//      elem.get match {
//        case entity: Entity => entity.setID(newValue)
//        case intAttr: IntAttribute => intAttr.setValue(newValue.replace(" ", "").toInt)
//        case stringAttr: StringAttribute => stringAttr.setValue(newValue)
//        case _ => elem.get
//      }
//    }
//
//    def setNewEntity(entity: Option[Entity]): Callback = $.modState(_.copy(newEntity = entity))
//
//    def setNewRelation(relationType: Option[RelationType]): Callback = $.modState(_.copy(newRelation = relationType))
//
//    def setNewAttribute(attribute: Option[Attribute]): Callback = $.modState(_.copy(newAttribute = attribute))
//
//    def addElemModalStyle(P: Props, S: State) = Seq(
//      <.h4(
//        "Do you want to add the following?",
//        ^.textAlign.center
//      ),
//      <.dl(
//        ^.className := "dl-horizontal",
//        <.br,
//        <.dt(
//          P.modalProps.treeItem.entityToString,
//          ^.textAlign := "center",
//          ^.color := {
//            if (P.modalProps.treeItem.isInstanceOf[Attribute]) "#03EE7D" else "#047BEA"
//          }
//        ),
//        <.dd(
//          {
//            if (P.modalProps.treeItem.entityToString != "Model") P.modalProps.treeItem.contentToString else ""
//          }
//        ),
//        <.hr,
//        <.dt(
//          ^.textAlign := "center",
//          ^.color := "#FF3636",
//          "has"
//        ),
//        <.dd(
//
//        ),
//        <.hr,
//        <.dt(
//          P.modalProps.elemToAdd match {
//            case Some(e: Entity) => e.getType
//            case Some(e: IntAttribute) => e.getType
//            case Some(e: StringAttribute) => e.getType
//            case None => "Error"
//          },
//          ^.textAlign := "center",
//          ^.color := {
//            if (P.modalProps.elemToAdd.get.isEntity) "#047BEA" else "#03EE7D"
//          }
//        ),
//        <.dd(
//          if (P.modalProps.elemToAdd.get.isIntAttribute) {
//            <.input(
//              ^.value := S.input,
//              ^.className := "form-control",
//              ^.width := "60%",
//              ^.borderRadius := "5px",
//              ^.autoFocus := "true",
//              ^.onChange ==> intInputChanged,
//              ^.maxLength := "9",
//              ^.placeholder := "Number"
//            )
//          } else {
//            <.textarea(
//              ^.className := "form-control",
//              ^.width := "95%",
//              ^.maxWidth := "95%",
//              ^.maxHeight := "200px",
//              ^.border := "1px solid #CCC",
//              ^.borderRadius := "5px",
//              ^.autoFocus := "true",
//              ^.placeholder := {
//                if (P.modalProps.elemToAdd.get.isEntity) "Id" else "Description"
//              },
//              ^.onChange ==> inputChanged
//            )
//          }
//        ),
//        <.br
//      ),
//      <.div(
//        ^.padding := "5px",
//        ^.display.flex,
//        ^.justifyContent.spaceBetween,
//        <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick ==> onClose(P)),
//        <.button("Add", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.disabled := S.input.isEmpty, ^.onClick ==> addElem(P, S))
//      )
//    )
//
//    def editModalStyle(P: Props, S: State) = Seq(
//      <.h4(
//        "Edit element",
//        ^.textAlign.center
//      ),
//      <.dl(
//        <.br,
//        ^.className := "dl-horizontal",
//        <.dt(
//          P.modalProps.treeItem.item match {
//            case intAttr: IntAttribute => <.div(
//              ^.textAlign := "center",
//              ^.color := "#03EE7D",
//              AttributeSelect(intAttr.getType, isIntAttr = true, setNewAttribute)
//            )
//            case stringAttr: StringAttribute => <.div(
//              ^.textAlign := "center",
//              ^.color := "#03EE7D",
//              AttributeSelect(stringAttr.getType, isIntAttr = false, setNewAttribute)
//            )
//            case _: Entity => EntitySelect(P.modalProps.treeItem.entityToString, setNewEntity, isModelValue = false)
//          }),
//        <.dd(
//          if (P.modalProps.treeItem.item.isInstanceOf[IntAttribute]) {
//            <.input(
//              ^.value := S.input,
//              ^.className := "form-control",
//              ^.maxLength := "9",
//              ^.width := "60%",
//              ^.borderRadius := "5px",
//              ^.autoFocus := "true",
//              ^.onChange ==> intInputChanged
//            )
//          } else {
//            <.textarea(
//              ^.rows := {
//                if (S.input.length < 28) "2" else "4"
//              },
//              ^.className := "form-control",
//              ^.width := "95%",
//              ^.maxWidth := "95%",
//              ^.value := S.input,
//              ^.autoFocus := "true",
//              ^.maxHeight := "200px",
//              ^.border := "1px solid #CCC",
//              ^.borderRadius := "5px",
//              ^.onChange ==> inputChanged
//            )
//          }
//        ),
//        P.modalProps.treeItem.children.nonEmpty ?= <.hr,
//        <.dt(
//          ^.textAlign := "center",
//          ^.color := "#FF3636",
//          ^.position.relative,
//          if (P.modalProps.treeItem.children.nonEmpty) {
//            RelationSelect(P.modalProps.treeItem.linkToString, P.modalProps.dispatch, None, isModelValue = false, Some(setNewRelation), None)
//          } else {
//            <.div()
//          }
//        ),
//        <.dd(
//        ),
//        P.modalProps.treeItem.children.nonEmpty ?= <.hr,
//        P.modalProps.treeItem.children.map(x => {
//          Seq(
//            <.dt(
//              x.entityToString.replaceAll("TreeItem", ""),
//              ^.textAlign := "center",
//              ^.paddingRight := "3.5%",
//              ^.color := {
//                if (x.item.isInstanceOf[StringAttribute] || x.item.isInstanceOf[IntAttribute]) "#03EE7D" else "#047BEA"
//              }
//            ),
//            <.dd(
//              x.contentToString
//            )
//          )
//        }),
//        <.br
//      ),
//      <.div(
//        ^.padding := "5px",
//        ^.display.flex,
//        ^.justifyContent.spaceBetween,
//        <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick ==> onClose(P)),
//        <.button("Save Changes", ^.className := "btn btn-success pull-right", ^.disabled := S.input.isEmpty, ^.bottom := "0px", ^.onClick ==> onSave(P, S))
//      )
//    )
//
//    def deleteElemModalStyle(P: Props, S: State) =
//      if (P.modalProps.treeItem.item != "Model") {
//        Seq(
//          <.h4(
//            "Do you want to delete the following?",
//            ^.textAlign.center
//          ),
//
//          <.dl(
//            <.br,
//            ^.className := "dl-horizontal",
//            <.dt(
//              ^.textAlign := "center",
//              ^.color := {
//                if (P.modalProps.treeItem.item.isInstanceOf[IntAttribute] || P.modalProps.treeItem.item.isInstanceOf[StringAttribute]) "#03EE7D" else "#047BEA"
//              },
//              P.modalProps.treeItem.entityToString),
//            <.dd(
//              P.modalProps.treeItem.contentToString
//            ),
//            P.modalProps.treeItem.children.nonEmpty ?= <.hr,
//            <.dt(
//              ^.textAlign := "center",
//              ^.color := "#FF3636",
//              P.modalProps.treeItem.linkToString
//            ),
//            <.dl(
//
//            ),
//            P.modalProps.treeItem.children.nonEmpty ?= <.br,
//            P.modalProps.treeItem.children.nonEmpty ?= <.hr,
//            P.modalProps.treeItem.children.map(child => {
//              Seq(
//                <.dt(
//                  child.entityToString.replaceAll("TreeItem", ""),
//                  ^.textAlign := "center",
//                  ^.color := {
//                    if (child.item.isInstanceOf[IntAttribute] || child.item.isInstanceOf[StringAttribute]) "#03EE7D" else "#047BEA"
//                  }
//                ),
//                <.dd(
//                  child.contentToString
//                )
//              )
//            }),
//            P.modalProps.treeItem.children.nonEmpty ?= <.br
//          ),
//          <.div(
//            ^.padding := "5px",
//            ^.display.flex,
//            ^.justifyContent.spaceBetween,
//            <.button("Cancel", ^.className := "btn btn-default", ^.bottom := "0px", ^.onClick ==> onClose(P)),
//            <.button("Delete", ^.className := "btn btn-danger", ^.bottom := "0px", ^.onClick ==> onDelete(P))
//          )
//        )
//      } else {
//        Seq(
//          <.h4(
//            "Do you want to delete the entire model?",
//            ^.textAlign.center
//          ),
//          <.br,
//          <.div(
//            ^.padding := "5px",
//            ^.display.flex,
//            ^.justifyContent.spaceBetween,
//            <.button("Cancel", ^.className := "btn btn-default", ^.bottom := "0px", ^.onClick ==> onClose(P)),
//            <.button("Delete", ^.className := "btn btn-danger", ^.autoFocus := "true", ^.bottom := "0px", ^.onClick ==> onDelete(P))
//          )
//        )
//      }
//
//
//    def onDelete(P: Props)(event: ReactEvent): Callback = {
//      P.modalProps.treeItem.item match {
//        case _: String =>
//          P.modalProps.dispatch(RemoveElem(P.modalProps.path)) >> P.onClose(event)
//        case _ =>
//          P.modalProps.dispatch(RemoveElem(P.modalProps.path)) >> P.modalProps.dispatch(RemoveEmptyRelation(P.modalProps.path.init)) >> P.onClose(event)
//      }
//    }
//
//
//    def initStates(newProps: Props): Callback = {
//      if (newProps.modalProps.treeItem != null) {
//        val newInput = if (newProps.modalProps.elemToAdd.isEmpty && newProps.modalProps.treeItem.item != "Model") newProps.modalProps.treeItem.contentToString else ""
//
//
//        newProps.modalProps.treeItem.item match {
//          case entity: Entity =>
//            $.modState(_.copy(input = newInput, newEntity = Some(entity), newRelation = newProps.modalProps.treeItem.link, newAttribute = None))
//          case attribute: IntAttribute =>
//            $.modState(_.copy(input = newInput, newEntity = None, newRelation = newProps.modalProps.treeItem.link, newAttribute = Some(attribute)))
//          case attribute: StringAttribute =>
//            $.modState(_.copy(input = newInput, newEntity = None, newRelation = newProps.modalProps.treeItem.link, newAttribute = Some(attribute)))
//          case _ =>
//            $.modState(_.copy(input = newInput, newEntity = None, newRelation = None, newAttribute = None))
//        }
//      } else {
//        Callback()
//      }
//    }
//  }
//
//
//  val component = ReactComponentB[Props]("Modal")
//    .initialState(State(input = "", newEntity = None, newRelation = None, newAttribute = None))
//    .renderBackend[Backend]
//    .componentWillReceiveProps(i => i.$.backend.initStates(i.nextProps))
//    .build
//
//  def apply(isOpen: Boolean, onClose: ReactEvent => Callback, modalProps: webApp.ModelProps)
//  = component.set()(Props(isOpen, onClose, modalProps))
//}
