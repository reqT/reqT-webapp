package example

import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent, _}


object Modal {

  //case class State(isOpen: Boolean)
//
  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback)

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


  def close(onClose:ReactEvent => Callback)(e: ReactEvent): Callback ={
    e.preventDefault()
    onClose(e)
  }



  class Backend(t: BackendScope[Props, _]) {
    def render(p: Props) =
      if (p.isOpen) {
        <.div(
          <.div(
            modalStyle
            //p.genContent()
          ),
          <.div(
            backdropStyle,
            ^.onClick ==> close(p.onClose)
          )
        )
      }else { <.div()}
  }

  val component = ReactComponentB[Props]("Modal")
    .stateless //initialState(State(isOpen = false))
    .renderBackend[Backend]
    .build



  def apply(isOpen: Boolean, onClose: ReactEvent => Callback) = component.set()(Props(isOpen, onClose))
}
