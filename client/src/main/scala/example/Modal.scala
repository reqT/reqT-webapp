package example

import diode.Action
import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent}
import org.scalajs.dom.ext.KeyCode
import japgolly.scalajs.react._


object Modal {

  case class State(input: String)

  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback, content: Seq[TagMod], dispatch: Action => Callback, action: String => Action)

  def modalStyle = Seq(
    ^.position := "absolute",
    ^.border := "1px solid #CCC",
    ^.borderRadius := "5px",
    ^.top := "40%",
    ^.left := "50%",
    ^.transform := "translate(-50%,-50%)",
    ^.zIndex := "9999",
    ^.background := "#FFF" ,
    ^.width:= "400px",
    ^.height:= "500px",
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



  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S:State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P,S),
            <.div(
                modalStyle,
                P.content,
              <.input(
                ^.className := "form-control",
                ^.placeholder := "Name..",
                ^.position.absolute,
                ^.bottom := 0,
                ^.width := "95%",
                ^.marginBottom := "10px",
                ^.marginLeft := "5px",
                ^.marginRight := "5px",
                ^.autoFocus := true,
                ^.onChange ==> onChange
              )
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


    def onChange(event: ReactEventI): Callback ={
      val newInput = event.target.value
      $.modState(_.copy(input = newInput))
    }

    def handleKeyDown(P: Props, S: State)(event: ReactKeyboardEventI): Callback = {
      if (event.nativeEvent.keyCode == KeyCode.Enter ){
        event.preventDefault()
        P.dispatch(P.action(S.input)) >> resetInput >> P.onClose(event)
      } else if ( event.nativeEvent.keyCode == KeyCode.Escape){
        event.preventDefault()
        resetInput >> P.onClose(event)
      }
      else
        Callback()
    }

  }

  val component = ReactComponentB[Props]("Modal")
    .initialState(State(input = ""))
    .renderBackend[Backend]
    .build



  def apply(isOpen: Boolean, onClose: ReactEvent => Callback, content: Seq[TagMod], dispatch: Action => Callback, action: String => Action)
      = component.set()(Props(isOpen, onClose, content, dispatch, action))
}
