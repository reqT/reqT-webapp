package example

import diode.Action
import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent}
import org.scalajs.dom.ext.KeyCode
import japgolly.scalajs.react._


object Modal {

  sealed trait ModalType
  case object EMPTY_MODAL extends ModalType
  case object ADD_ENTITY_MODAL extends ModalType
  case object EDIT_MODAL extends ModalType
  case object DELETE_MODAL extends ModalType

  case class State(input: String)

  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback, modalType: ModalType, treeItem: TreeItem, dispatch: Action => Callback, path: Seq[String], elemToAdd: Option[Elem])

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
    ^.paddingLeft := "15px"
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

  def editModalStyle(P: Props, rerender: () => Callback) = Seq(
    ^.width:= "400px",
    <.h4(
      "Edit element",
      ^.textAlign.center
    ),
    <.br,
    <.dl(
      ^.className := "dl-horizontal",
      <.dt(
        if(P.treeItem.isInstanceOf[Attribute[Any]]){
          <.div(
            ^.textAlign := "center",
            ^.color := "#03EE7D",
            P.treeItem.entityToString
          )
        } else {
          <.div()
          //EntitySelect(P.treeItem.item, P.dispatch, NoAction)
        }),
      <.dd(
        P.treeItem.contentToString
      ),

      <.hr,
      <.dt(
        ^.textAlign := "center",
        ^.color := "#FF3636",
        ^.position.relative,
        RelationSelect(P.treeItem.linkToString,P.dispatch, UpdateRelation(P.path, P.treeItem.item.asInstanceOf[Entity].id, _: Option[RelationType]), Some(rerender))
      ),
      <.dd(

      ),
      <.hr,
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
      <.br
    ),
    <.div(
      ^.padding := "5px",
      ^.display.flex,
      ^.justifyContent.center,
      <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick ==> P.onClose),
      <.button("Save Changes", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.onClick ==> onSave(P))
    )
  )

  def deleteElemModalStyle(P : Props) = Seq(
    ^.width:= "400px",
    <.h4(
      "Do you want to delete the following?",
      ^.textAlign.center
    ),

    <.dl(
      <.br,
      ^.border := "1px solid #CCC",
      ^.borderRadius := "5px",
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
      ^.justifyContent.center,
      <.button("Cancel", ^.className := "btn btn-default", ^.bottom := "0px", ^.onClick ==> P.onClose),
      <.button("Delete", ^.autoFocus := "true", ^.className := "btn btn-danger", ^.bottom := "0px", ^.onClick ==> onDelete(P))
    )
  )

  def onDelete(P: Props)(event: ReactEvent): Callback = {
    P.dispatch(RemoveElem(P.path)) >> P.dispatch(RemoveEmptyRelation(P.path.init)) >> P.onClose(event)
  }

  def onSave(P: Props)(event: ReactEvent): Callback = {
    Callback()
  }


  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S:State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P,S),
            <.div(
              modalStyle,
              getModalStyle(P, S, rerender)
            ),
            <.div(
                backdropStyle,
                ^.onClick ==> onClickOutsideModal(P, S)
            )
        )
      }else
        <.div()

    def resetInput = $.modState(_.copy(input = ""))

    def inputChanged(e : ReactEventI): Callback = {
      val newInput = e.target.value
      $.modState(_.copy(input = newInput))
    }



    def onClickOutsideModal(P: Props, S:State)(e: ReactEvent): Callback = resetInput >> P.onClose(e)

    def getModalStyle(P: Props, S:State, rerender: () => Callback) : Seq[TagMod] = {
      P.modalType match{
        case EDIT_MODAL => editModalStyle(P, rerender)
        case ADD_ENTITY_MODAL => addElemModalStyle(P, S)
        case DELETE_MODAL => deleteElemModalStyle(P)
        case EMPTY_MODAL => Seq("Error 404")
      }
    }


    def handleKeyDown(P: Props, S: State)(event: ReactKeyboardEventI): Callback = {
     if ( event.nativeEvent.keyCode == KeyCode.Escape){
//        event.preventDefault()
//        resetInput >> P.onClose(event)
        P.onClose(event)
      }
      else
        Callback()
    }

    def rerender() = {
      println("Hej")
      $.setState(s = State(""))
    }

    def addElem(P: Props, S: State)(e: ReactEvent): Callback = P.dispatch(AddElem(P.path, prepareElem(S.input, P.elemToAdd), has)) >> P.onClose(e)

    def prepareElem(newValue: String, elem: Option[Elem]): Elem = {
      elem.get match {
        case entity: Entity => entity.setID(newValue)
        case intAttr: IntAttribute => intAttr.setValue(newValue.replace(" ","").toInt)
        case stringAttr: StringAttribute => stringAttr.setValue(newValue)
      }
    }


    def addElemModalStyle(P : Props, S: State) = Seq(
      ^.width:= "400px",
      <.h4(
        "Do you want to add the following?",
        ^.textAlign.center
      ),
      <.dl(
        ^.border := "1px solid #CCC",
        ^.borderRadius := "5px",
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
              ^.className := "form-control",
              ^.width := "60%",
              ^.borderRadius := "5px",
              ^.autoFocus := "true",
              ^.onChange ==> inputChanged,
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
        ^.justifyContent.center,
        <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick ==> P.onClose),
        <.button("Add", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.onClick ==> addElem(P, S))
      )
    )


  }


  val component = ReactComponentB[Props]("Modal")
    .initialState(State(input = ""))
    .renderBackend[Backend]
    .build



  def apply(isOpen: Boolean, onClose: ReactEvent => Callback, modalType: ModalType, treeItem: TreeItem, dispatch: Action => Callback, path: Seq[String], elemToAdd: Option[Elem])
      = component.set()(Props(isOpen, onClose, modalType, treeItem, dispatch, path, elemToAdd))
}
