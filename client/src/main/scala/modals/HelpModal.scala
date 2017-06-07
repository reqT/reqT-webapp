package modals

import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactKeyboardEventI, _}
import org.scalajs.dom.ext.KeyCode


object HelpModal {

  val textAreaStyle = Seq(
    ^.className := "form-control",
    ^.rows := 6,
    ^.maxWidth := "100%",
    ^.maxHeight := "200px",
    ^.border := "1px solid #CCC",
    ^.borderRadius := "5px",
    ^.background := "#FFF",
    ^.autoFocus := "true",
    ^.readOnly := "true",
    ^.selected := "true"
  )

  val buttonAreaStyle = Seq(
    ^.padding := "5px",
    ^.display.flex,
    ^.justifyContent.spaceBetween
  )

  val modalStyle = Seq(
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

  val backdropStyle = Seq(
    ^.position := "absolute",
    ^.width := "100%",
    ^.height := "100%",
    ^.top := "0px",
    ^.left := "0px",
    ^.zIndex := "9998",
    ^.background := "#CCC",
    ^.opacity := "0.5"
  )

  case class State()

  case class Props(isOpen: Boolean, onClose: () => Callback)

  class Backend($: BackendScope[Props, State]) {

    def onClose(P: Props): Callback = P.onClose()

    def handleKeyDown(P: Props)(e: ReactKeyboardEventI): Callback = {
      if (e.nativeEvent.keyCode == KeyCode.Escape) {
        onClose(P)
      }
      else
        Callback()
    }


    def render(P: Props, S: State) = {
      if (P.isOpen)
        <.div(
          ^.onKeyDown ==> handleKeyDown(P),
          copyModal(P),
          <.div(
            backdropStyle,
            ^.onClick --> onClose(P)
          )
        )
      else
        <.div()
    }


    val copyModal = ReactComponentB[Props]("copyModal")
      .render($ =>
        <.div(
          modalStyle,
          <.h4(
            "Help",
            ^.textAlign.center
          ),
          <.dd(
          ),
          <.div(
            buttonAreaStyle,
            <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick --> onClose($.props))
          )
        )
      ).build

  }


  val component = ReactComponentB[Props]("Modal")
    .initialState(State())
    .renderBackend[Backend]
    .build


  def apply(isOpen: Boolean, onClose: () => Callback) = component.set()(Props(isOpen, onClose))

}
