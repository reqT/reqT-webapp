package example

import diode.Action
import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent}
import org.scalajs.dom.ext.KeyCode
import japgolly.scalajs.react._


object Modal {


  case object EMPTY_MODAL extends ModalType
  case object ADD_ELEM_MODAL extends ModalType
  case object EDIT_MODAL extends ModalType
  case object DELETE_MODAL extends ModalType

  def modalStyle = Seq(
    ^.padding := "5px",
    ^.position := "absolute",
    ^.border := "1px solid #CCC",
    ^.borderRadius := "5px",
    ^.top := "40%",
    ^.left := "50%",
    ^.transform := "translate(-50%,-50%)",
    ^.zIndex := "9999",
    ^.background := "#FFF" ,
    ^.paddingBottom := "15px",
    ^.paddingRight := "15px" ,
    ^.paddingTop := "15px",
    ^.paddingLeft := "15px",
    ^.boxShadow := "rgba(0, 0, 0, 0.2) 5px 6px 12px 0px"
  )

  def backdropStyle = Seq(
    ^.position := "absolute",
    ^.width := "100%",
    ^.height := "100%",
    ^.top := "0px",
    ^.left := "0px",
    ^.zIndex := "9998",
    ^.background := "#CCC",
    ^.opacity := "0.5"
  )

  def apply(isOpen: Boolean, onClose: ReactEvent => Callback, modalType: ModalType, treeItem: TreeItem, dispatch: Action => Callback, path: Seq[String], elemToAdd: Option[Elem])
      = component.set()(Props(isOpen, onClose, modalType, treeItem, dispatch, path, elemToAdd))

  sealed trait ModalType

  case class State(input: String, newEntity: Option[Entity], newRelation: Option[RelationType], newAttribute: Option[Attribute[Any]])

  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback, modalType: ModalType, treeItem: TreeItem, dispatch: Action => Callback, path: Seq[String], elemToAdd: Option[Elem])

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S:State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P,S),
            <.div(
              modalStyle,
              getModalStyle(P, S)
            ),
            <.div(
                backdropStyle,
                ^.onClick ==> onClose(P)
            )
        )
      }else
        <.div()

    def resetState: Callback = $.modState(_.copy(input = "", newEntity = None, newRelation = None, newAttribute = None))

    def onClose(P: Props)(e: ReactEvent): Callback = P.onClose(e) >> resetState

    def onSave(P: Props, S: State)(e: ReactEvent): Callback = {
      P.treeItem.item match {
        case entity: Entity => if(entity.hasRelation){
          S.newEntity.get.setID(S.input)
          S.newEntity.get.hasRelation = true
          P.dispatch(UpdateEntireRelation(path = P.path, newEntity = S.newEntity.get, S.newRelation)) >> onClose(P)(e)
//          P.dispatch(UpdateEntity(path = P.path, newEntity = S.newEntity.get)) >> P.dispatch(UpdateRelation(path = P.path, S.input, S.newRelation)) >> onClose(P)(e)
        }else{
          println(S.input)
          S.newEntity.get.setID(S.input)
          P.dispatch(UpdateEntity(path = P.path, newEntity = S.newEntity.get)) >> onClose(P)(e)
        }

        case _: IntAttribute =>
           P.dispatch(UpdateIntAttribute(path = P.path, S.newAttribute.getOrElse(P.treeItem.item).asInstanceOf[IntAttribute].setValue(S.input.toInt))) >> onClose(P)(e)
        case _: StringAttribute => P.dispatch(UpdateStringAttribute(path = P.path, S.newAttribute.getOrElse(P.treeItem.item).asInstanceOf[StringAttribute].setValue(S.input))) >> onClose(P)(e)

      }
    }

    def inputChanged(e : ReactEventI): Callback = {
      val newInput = e.target.value
      $.modState(_.copy(input = newInput))
    }

    def intInputChanged(e : ReactEventI): Callback = {
      val newInput = e.target.value
      $.modState(_.copy(input = newInput.replaceAll( "[^\\d]", "" )))
    }

    def getModalStyle(P: Props, S:State) : Seq[TagMod] = {
      P.modalType match{
        case EDIT_MODAL => editModalStyle(P, S)
        case ADD_ELEM_MODAL => addElemModalStyle(P, S)
        case DELETE_MODAL => deleteElemModalStyle(P, S)
        case EMPTY_MODAL => Seq("Error 404")
      }
    }

    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventI): Callback = {
     if (e.nativeEvent.keyCode == KeyCode.Escape){
        onClose(P)(e)
      }
      else
        Callback()
    }

    def addElem(P: Props, S: State)(e: ReactEvent): Callback = P.dispatch(AddElem(P.path, prepareElem(S.input, P.elemToAdd), has)) >> onClose(P)(e)

    def prepareElem(newValue: String, elem: Option[Elem]): Elem = {
      elem.get match {
        case entity: Entity => entity.setID(newValue)
        case intAttr: IntAttribute => intAttr.setValue(newValue.replace(" ","").toInt)
        case stringAttr: StringAttribute => stringAttr.setValue(newValue)
      }
    }

    def setNewEntity(entity: Option[Entity]): Callback = $.modState(_.copy(newEntity = entity))

    def setNewRelation(relationType: Option[RelationType]): Callback = $.modState(_.copy(newRelation = relationType))

    def setNewAttribute(attribute: Option[Attribute[Any]]): Callback = $.modState(_.copy(newAttribute = attribute))

    def addElemModalStyle(P : Props, S: State) = Seq(
      ^.width:= "400px",
      <.h4(
        "Do you want to add the following?",
        ^.textAlign.center
      ),
      <.dl(
        ^.className := "dl-horizontal",
        <.br,
        <.dt(
          ^.textAlign := "center",
          ^.color := { if (P.treeItem.isInstanceOf[Attribute[Any]]) "#03EE7D" else "#047BEA" },
          P.treeItem.entityToString),
        <.dd(
          P.treeItem.contentToString
        ),
        <.hr,
        <.dt(
          ^.textAlign := "center",
          ^.color := "#FF3636",
          "has"
        ),
        <.dd(

        ),
        <.hr,
        <.dt(
          P.elemToAdd.get.toString.split('(').head,
          ^.textAlign := "center",
          ^.color := { if (P.elemToAdd.get.isEntity) "#047BEA" else "#03EE7D"}
        ),
        <.dd(
          if(P.elemToAdd.get.isIntAttribute){
            <.input(
              ^.value := S.input,
              ^.className := "form-control",
              ^.width := "60%",
              ^.borderRadius := "5px",
              ^.autoFocus := "true",
              ^.onChange ==> intInputChanged,
              ^.maxLength := "9",
              ^.placeholder := "Number"
            )
          } else {
            <.textarea(
            ^.className := "form-control",
            ^.width := "95%",
            ^.maxWidth := "95%",
            ^.maxHeight := "200px",
            ^.border := "1px solid #CCC",
            ^.borderRadius := "5px",
            ^.autoFocus := "true",
            ^.placeholder := {if(P.elemToAdd.get.isEntity) "Id" else "Description" },
            ^.onChange ==> inputChanged
          )}
        ),
        <.br
      ),
      <.div(
        ^.padding := "5px",
        ^.display.flex,
        ^.justifyContent.spaceBetween,
        <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick ==> onClose(P)),
        <.button("Add", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.onClick ==> addElem(P, S))
      )
    )

    def editModalStyle(P: Props, S: State) = Seq(
      ^.width:= "400px",
      <.h4(
        "Edit element",
        ^.textAlign.center
      ),
      <.dl(
        <.br,
        ^.className := "dl-horizontal",
        <.dt(
          P.treeItem.item match {
            case intAttr: IntAttribute =>  <.div(
              ^.textAlign := "center",
              ^.color := "#03EE7D",
              AttributeSelect(intAttr.toString.split('(').head, isIntAttr = true, setNewAttribute)
            )
            case stringAttr: StringAttribute => <.div(
              ^.textAlign := "center",
              ^.color := "#03EE7D",
              AttributeSelect(stringAttr.toString.split('(').head, isIntAttr = false, setNewAttribute)
            )
            case _: Entity => EntitySelect(P.treeItem.entityToString, setNewEntity, isModelValue = false)
          }),
        <.dd(
          if(P.treeItem.item.isInstanceOf[IntAttribute]){
            <.input(
              ^.value := S.input,
              ^.className := "form-control",
              ^.maxLength := "9",
              ^.width := "60%",
              ^.borderRadius := "5px",
              ^.autoFocus := "true",
              ^.onChange ==> intInputChanged
            )
          }else{
            <.textarea(
            ^.rows := "1",
            ^.className := "form-control",
            ^.width := "95%",
            ^.maxWidth := "95%",
            ^.value := S.input,
            ^.autoFocus := "true",
            ^.maxHeight := "200px",
            ^.border := "1px solid #CCC",
            ^.borderRadius := "5px",
            ^.onChange ==> inputChanged
          )}
        ),
        P.treeItem.children.nonEmpty ?= <.hr,
        <.dt(
          ^.textAlign := "center",
          ^.color := "#FF3636",
          ^.position.relative,
          if(P.treeItem.children.nonEmpty) {
            RelationSelect(P.treeItem.linkToString, P.dispatch, None, isModelValue = false, Some(setNewRelation))
          } else {
            <.div()
          }
        ),
        <.dd(
        ),
        P.treeItem.children.nonEmpty ?= <.hr,
        P.treeItem.children.map(x => {
          Seq(
            <.dt(
              x.entityToString.replaceAll("TreeItem", ""),
              ^.textAlign := "center",
              ^.paddingRight := "3.5%",
              ^.color := { if (x.item.isInstanceOf[Attribute[Any]]) "#03EE7D" else "#047BEA" }
            ),
            <.dd(
              x.contentToString
            )
          )
        }),
        <.br
      ),
      <.div(
        ^.padding := "5px",
        ^.display.flex,
        ^.justifyContent.spaceBetween,
        <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick ==> onClose(P)),
        <.button("Save Changes", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.onClick ==> onSave(P, S))
      )
    )

    def deleteElemModalStyle(P : Props, S: State) = Seq(
      ^.width:= "400px",
      <.h4(
        "Do you want to delete the following?",
        ^.textAlign.center
      ),

      <.dl(
        <.br,
        ^.className := "dl-horizontal",
        <.dt(
          ^.textAlign := "center",
          ^.color := { if (P.treeItem.isInstanceOf[Attribute[Any]]) "#03EE7D" else "#047BEA" },
          P.treeItem.entityToString),
        <.dd(
          P.treeItem.contentToString
        ),
        P.treeItem.children.nonEmpty ?= <.hr,
        <.dt(
          ^.textAlign := "center",
          ^.color := "#FF3636",
          P.treeItem.linkToString
        ),
        <.dl(

        ),
        P.treeItem.children.nonEmpty ?= <.br,
        P.treeItem.children.nonEmpty ?= <.hr,
        P.treeItem.children.map(x => {
          Seq(
            <.dt(
              x.entityToString.replaceAll("TreeItem", ""),
              ^.textAlign := "center",
              ^.color := { if (x.item.isInstanceOf[Attribute[Any]]) "#03EE7D" else "#047BEA" }
            ),
            <.dd(
              x.contentToString
            )
          )
        }),
        P.treeItem.children.nonEmpty ?= <.br
      ),
      <.div(
        ^.padding := "5px",
        ^.display.flex,
        ^.justifyContent.spaceBetween,
        <.button("Cancel", ^.className := "btn btn-default", ^.bottom := "0px", ^.onClick ==> onClose(P)),
        <.button("Delete", ^.className := "btn btn-danger", ^.bottom := "0px", ^.onClick ==> onDelete(P))
      )
    )

    def onDelete(P: Props)(event: ReactEvent): Callback = P.dispatch(RemoveElem(P.path)) >> P.dispatch(RemoveEmptyRelation(P.path.init)) >> P.onClose(event)


    def initStates(newProps: Props): Callback = {
      newProps.treeItem.item match {
        case entity: Entity =>
          $.modState(_.copy(input = newProps.treeItem.contentToString, newEntity = Some(entity), newRelation = newProps.treeItem.link, newAttribute = None))
        case attribute: Attribute[Any] =>
          $.modState(_.copy(input = newProps.treeItem.contentToString, newEntity = None, newRelation = newProps.treeItem.link, newAttribute = Some(attribute)))
//        case stringAttr: StringAttribute =>
//          $.modState(_.copy(input = newProps.treeItem.contentToString, newEntity = None, newRelation = newProps.treeItem.link, newAttribute = Some(stringAttr)))
      }
    }
  }



  val component = ReactComponentB[Props]("Modal")
    .initialState(State(input = "", newEntity = None, newRelation = None, newAttribute = None))
    .renderBackend[Backend]
    .componentWillReceiveProps(i => i.$.backend.initStates(i.nextProps))
    .build

}
