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
  case object ADD_INTATTR_MODAL extends ModalType
  case object ADD_STRINGATTR_MODAL extends ModalType
  case object EDIT_MODAL extends ModalType
  case object DELETE_MODAL extends ModalType

  case class State(input: String)

  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback, modalType: ModalType, treeItem: TreeItem, dispatch: Action => Callback, path: Seq[String])

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

  def onChangeEntity(P: Props) = {

  }

  def addEntityModalStyle(P : Props) = Seq(
    ^.width:= "400px",
//    ^.height:= "500px",
    <.dl(
      ^.className := "dl-horizontal",
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
        P.treeItem.linkToString
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
      })

    )
  )

  def addStringAttrModalStyle(P : Props) = Seq(

  )

  def addIntAttrModalStyle(P : Props) = Seq(

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
      <.hr,
      <.dt(
        ^.textAlign := "center",
        ^.color := "#FF3636",
        P.treeItem.linkToString
      ),
      <.dl(

      ),
      <.br,
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
      <.button("Cancel", ^.className := "btn btn-default", ^.bottom := "0px", ^.onClick ==> P.onClose),
      <.button("Delete", ^.className := "btn btn-danger", ^.bottom := "0px", ^.onClick ==> onDelete(P))
    )
  )

  def onDelete(P: Props)(event: ReactEvent): Callback = {
    P.dispatch(RemoveElem(P.path)) >> P.dispatch(RemoveEmptyRelation(P.path.init)) >> P.onClose(event)
  }

  def onSave(P: Props)(event: ReactEvent): Callback = {
    Callback()
  }

  def getModalStyle(P: Props, rerender: () => Callback) : Seq[TagMod] = {
    P.modalType match{
      case EDIT_MODAL => editModalStyle(P, rerender)
      case ADD_ENTITY_MODAL => addEntityModalStyle(P)
      case ADD_STRINGATTR_MODAL => addStringAttrModalStyle(P)
      case ADD_INTATTR_MODAL => addIntAttrModalStyle(P)
      case DELETE_MODAL => deleteElemModalStyle(P)
      case EMPTY_MODAL => Seq("Error 404")
    }
  }


  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S:State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P,S),
            <.div(
              modalStyle,
              getModalStyle(P, rerender)
//              <.input(
//                ^.className := "form-control",
//                ^.placeholder := "Name..",
//                ^.position.absolute,
//                ^.bottom := 0,
//                ^.width := "95%",
//                ^.marginBottom := "10px",
//                ^.marginLeft := "5px",
//                ^.marginRight := "5px",
//                ^.autoFocus := true,
//                ^.onChange ==> onChange
//              )
            ),
            <.div(
                backdropStyle,
                ^.onClick ==> onClick(P, S)
            )
        )
      }else
        <.div()

    def resetInput = $.modState(_.copy(input = ""))

    def onClick(P: Props, S:State)(e: ReactEvent): Callback ={
      e.preventDefault()
      resetInput >> P.onClose(e)
    }

//    def onEntityChange(event: ReactEventI): Callback ={
//      val newInput = event.target.value
//      $.modState(_.copy(entityType = newInput))
//    }


    def onChange(event: ReactEventI): Callback ={
      val newInput = event.target.value
      $.modState(_.copy(input = newInput))
    }

    def handleKeyDown(P: Props, S: State)(event: ReactKeyboardEventI): Callback = {
      if (event.nativeEvent.keyCode == KeyCode.Enter ){
        event.preventDefault()
        P.dispatch(NoAction) >> resetInput >> P.onClose(event)
      } else if ( event.nativeEvent.keyCode == KeyCode.Escape){
        event.preventDefault()
        resetInput >> P.onClose(event)
      }
      else
        Callback()
    }

    def rerender() = {
      println("Hej")
      $.setState(s = State(""))
    }


  }


  val component = ReactComponentB[Props]("Modal")
    .initialState(State(input = ""))
    .renderBackend[Backend]
    .build



  def apply(isOpen: Boolean, onClose: ReactEvent => Callback, modalType: ModalType, treeItem: TreeItem, dispatch: Action => Callback, path: Seq[String] )
      = component.set()(Props(isOpen, onClose, modalType, treeItem, dispatch,  path ))
}
