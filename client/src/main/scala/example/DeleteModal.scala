package example


import diode.Action
import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB}
import org.scalajs.dom.ext.KeyCode
import japgolly.scalajs.react._
import shared._


object DeleteModal {

  def modalStyle = Seq(
    ^.width := "400px",
    ^.padding := "5px",
    ^.position := "absolute",
    ^.border := "1px solid #CCC",
    ^.borderRadius := "5px",
    ^.top := "40%",
    ^.left := "50%",
    ^.transform := "translate(-50%,-50%)",
    ^.zIndex := "9999",
    ^.background := "#FFF",
    ^.paddingBottom := "15px",
    ^.paddingRight := "15px",
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

  def buttonAreaStyle = Seq(
    ^.width:= "95%",
    ^.padding := "20px",
    ^.display.flex,
    ^.justifyContent.spaceBetween
  )

  case class State()

  case class Props(isOpen: Boolean, onClose: Callback, treeItem: TreeItem = null, dispatch: (Action => Callback) = null, path: Seq[String] = Seq())

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S:State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P,S),
          <.div(
            modalStyle,
            deleteElemModalStyle(P,S)
          ),
          <.div(
            backdropStyle,
            ^.onClick --> P.onClose
          )
        )
      }else
        <.div()

    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventI): Callback = {
      if (e.nativeEvent.keyCode == KeyCode.Escape){
        P.onClose
      } else if(e.nativeEvent.keyCode == KeyCode.Enter){
        onDelete(P)
      }else{
        Callback()
      }
    }

    def deleteElemModalStyle(P : Props, S: State) =

      P.treeItem.item match {

        case "Model" => Seq(
          <.h5(
            "Do you want to delete the entire model?",
            ^.fontSize := "16px",
            ^.textAlign.center
          ),
//          <.br,
          <.div(
            buttonAreaStyle,
            <.button("Cancel", ^.className := "btn btn-default", ^.bottom := "0px", ^.onClick --> P.onClose),
            <.button("Delete", ^.className := "btn btn-danger",   ^.autoFocus := "true", ^.bottom := "0px", ^.onClick --> onDelete(P))
          )
        )

        case item => Seq(
          <.h5(
            "Do you want to delete the following?",
            ^.fontSize := "16px",
            ^.textAlign.center
          ),
          <.dl(
            <.br,
            ^.className := "dl-horizontal",
            <.dt(
              ^.textAlign := "center",
              ^.color := {
                if (item.isInstanceOf[IntAttribute] || item.isInstanceOf[StringAttribute]) "#03EE7D" else "#047BEA"
              },
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
              ^.className := "dl-horizontal"
            ),
            P.treeItem.children.nonEmpty ?= <.br,<.hr,
            P.treeItem.children.map(child => {
              Seq(
                <.dt(
                  child.entityToString.replaceAll("TreeItem", ""),
                  ^.textAlign := "center",
                  ^.color := {
                    if (child.item.isInstanceOf[IntAttribute] || child.item.isInstanceOf[StringAttribute]) "#03EE7D" else "#047BEA"
                  }
                ),
                <.dd(
                  child.contentToString
                )
              )
            }),
            P.treeItem.children.nonEmpty ?= <.br
          ),
          <.div(
            buttonAreaStyle,
            <.button("Cancel", ^.className := "btn btn-default", ^.bottom := "0px", ^.onClick --> P.onClose),
            <.button("Delete", ^.className := "btn btn-danger", ^.bottom := "0px", ^.onClick --> onDelete(P))
          )
        )
      }


    def onDelete(P: Props): Callback = {
      P.treeItem.item match {
        case _: String =>
          P.dispatch(RemoveElem(P.path)) >> P.onClose
        case _ =>
          P.dispatch(RemoveElem(P.path)) >> P.dispatch(RemoveEmptyRelation(P.path.init)) >> P.onClose
      }
    }

  }


  val component = ReactComponentB[Props]("Modal")
    .initialState(State())
    .renderBackend[Backend]
//    .componentWillReceiveProps(i => i.$.backend.initStates(i.nextProps))
    .build

  def apply(isOpen: Boolean, onClose: Callback, treeItem: TreeItem, dispatch: (Action => Callback), path: Seq[String])
  = component.set()(Props(isOpen, onClose, treeItem, dispatch, path))
}
