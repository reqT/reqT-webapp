package example


import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent, ReactKeyboardEventI}
import org.scalajs.dom.ext.KeyCode
import japgolly.scalajs.react._
import shared.{Attribute, Entity}

/**
  * Created by johan on 5/17/17.
  */
object NewModelModal {
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

  def buttonStyle = Seq(
    ^.width := "95%",
    ^.padding := "20px",
    ^.display.flex,
    ^.justifyContent.spaceBetween
  )

  def textAreaStyle = Seq(
    ^.className := "form-control",
    ^.rows := 6,
    ^.maxWidth := "100%",
    ^.maxHeight := "200px",
    ^.border := "1px solid #CCC",
    ^.borderRadius := "5px",
    ^.background := "#FFF",
    ^.autoFocus := "true",
    ^.readOnly := "true"
  )

  case class Props(isOpen: Boolean, onClose: () => Callback, saveModel: (String, shared.Tree) => Callback, tree: shared.Tree, modalType: String)

  case class State(newModelName: String)

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P, S),
          <.div(
            modalStyle,
            if (P.modalType == "rec") {
              <.div(
                <.h3("Result"),
                <.div(
                  <.textarea(
                    textAreaStyle,
                    ^.value := s"Model(${P.tree})"
                  )
                ),
                InputComponent(S),
                <.div(
                  buttonStyle,
                  <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick --> onClose(P)),
                  <.button("Save Result", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.onClick --> addModel(S.newModelName, P.tree, P))
                )
              )
            } else if(P.modalType == "save") {
              <.div(
                InputComponent(S),
                <.div(
                  buttonStyle,
                  <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick --> onClose(P)),
                  <.button("Add empty model", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.onClick --> addModel(S.newModelName, shared.Tree(Seq()),P)),
                  <.button("Add current model", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.onClick --> addModel(S.newModelName, P.tree, P))
                )
              )
            } else {
              <.div(
                InputComponent(S),
                <.div(
                  buttonStyle,
                  <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick --> onClose(P)),
                  <.button("Add Model", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.onClick --> addModel(S.newModelName, P.tree, P))
                )
              )
            }
          ),
          <.div(
            backdropStyle,
            ^.onClick --> onClose(P)
          )
        )

      } else
        <.div()

    def InputComponent = ReactComponentB[State]("InputComponent")
      .render($ =>
        <.div(
          <.h4(
            "Specify name of the new model"
          ),
          <.input(
            ^.autoFocus := "true",
            ^.`type` := "text",
            ^.className := "form-control",
            ^.placeholder := "Enter name",
            ^.value := $.props.newModelName,
            ^.onChange ==> onChange($.props)
          )
        )
      ).build

    def onChange(S: State)(e: ReactEventI): Callback = {
      e.preventDefault()
      val newName = e.target.value
      $.setState(s = S.copy(newModelName = newName))
    }

    def addModel(newName: String, tree: shared.Tree, P: Props): Callback = P.saveModel(newName,tree) >> onClose(P)

    def resetState: Callback = $.setState(State(""))

    def onClose(P: Props): Callback = P.onClose() >> resetState

    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventI): Callback = {
      if (e.nativeEvent.keyCode == KeyCode.Escape) {
        onClose(P)
      }
      else
        Callback()
    }
  }

  val component = ReactComponentB[Props]("Modal")
    .initialState(State(""))
    .renderBackend[Backend]
    .build


  def apply(isOpen: Boolean, onClose: () => Callback, saveModel: (String, shared.Tree) => Callback, tree: shared.Tree, modalType: String)
  = component.set()(Props(isOpen, onClose, saveModel, tree, modalType))
}
