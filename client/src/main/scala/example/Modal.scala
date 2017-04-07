package example

import japgolly.scalajs.react.vdom.prefix_<^.<
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent}
import org.scalajs.dom.ext.KeyCode

import java.lang.Class
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^.{^, _}


object Modal {

  case class State(input: String)

  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback, content: Seq[TagMod], setOutput: String => Unit)

  def modalStyle = Seq(
    ^.position := "absolute",
    ^.border := "1px solid",
    ^.borderRadius := "5px",
    ^.top := "50%",
    ^.left := "50%",
    ^.transform := "translate(-50%,-50%)",
    ^.zIndex := "9999",
    ^.background := "#FFF" ,
    ^.width:= "300px",
    ^.height:= "300px",
    ^.paddingBottom := "150px",
    ^.paddingRight := "150px"
  )

  def backdropStyle = Seq(
    ^.position := "absolute",
    ^.width := "100%",
    ^.height := "100%",
    ^.top := "0px",
    ^.left := "0px",
    ^.zIndex := "9998"
  )



  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S:State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P),
            <.div(
                modalStyle,
                P.content,
              <.input(
                ^.placeholder := "Name..",
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


    def onClick(P: Props, S:State)(e: ReactEvent): Callback ={
      e.preventDefault()
      P.setOutput(S.input)
      P.onClose(e)
    }


    def onChange(event: ReactEventI): Callback ={
      val newInput = event.target.value
      $.modState(_.copy(input = newInput))
    }

    def handleKeyDown(P: Props)(event: ReactKeyboardEventI): Callback = {
      if (event.nativeEvent.keyCode == KeyCode.Enter || event.nativeEvent.keyCode == KeyCode.Escape){
        event.preventDefault()
        P.onClose(event)
      }
      else
        Callback()
    }

  }

  val component = ReactComponentB[Props]("Modal")
    .initialState(State(input = ""))
    .renderBackend[Backend]
    .build



  def apply(isOpen: Boolean, onClose: ReactEvent => Callback, content: Seq[TagMod], setOutput: String => Unit)
      = component.set()(Props(isOpen, onClose, content, setOutput))
}
