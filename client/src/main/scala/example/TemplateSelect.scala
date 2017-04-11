package example

import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, _}
import diode.Action
import org.scalajs.dom.ext.KeyCode


object TemplateSelect {

  case class State(isOpen: Boolean = false)

  case class Props(dispatch: Action => Callback)

  def contentStyle = Seq(
    ^.marginTop := "11px",
    ^.height := "70px",
    ^.width := "100px",
    ^.position := "absolute",
    ^.backgroundColor:= "#f9f9f9",
    ^.minWidth := "200px",
    ^.zIndex := "9999"
  )

  def backdropStyle = Seq(
    ^.position := "absolute",
    ^.width := "1000px",
    ^.height := "1000px",
    ^.top := "0px",
    ^.left := "0px",
    ^.zIndex := "9998"
  )

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State)  =
          <.button(
            ^.className := "btn btn-default navbar-btn",
            ^.margin := "5px",
            ^.padding := "10px",
            "Templates ",
            ^.onClick --> $.modState(_.copy(isOpen = !S.isOpen)),
            <.span(^.className := "caret"),
            S.isOpen ?= dropdownList(P),
            S.isOpen ?= <.div(
              backdropStyle,
             // ^.onClick --> closeDropdown,
              ^.onKeyDown ==> handleKeyDown
            )
          )


    val dropdownList = ReactComponentB[Props]("dropdownList")
      .render(P =>
        <.div(
          contentStyle,
            <.li(
              ^.className := "list-group-item",
              ^.boxShadow := "0px 8px 16px 0px rgba(0,0,0,0.2)",
              "Template 1",
              ^.onClick ==> onClick(P.props)
            ),
            <.li(
              ^.className := "list-group-item",
              ^.boxShadow := "0px 8px 16px 0px rgba(0,0,0,0.2)",
              "Template 2",
              ^.onClick ==> onClick(P.props)
            ),
            <.li(
              ^.className := "list-group-item",
              ^.boxShadow := "0px 8px 16px 0px rgba(0,0,0,0.2)",
              "Template 3",
              ^.onClick ==> onClick(P.props)
            )
        )
      )
      .build


    def handleKeyDown(event: ReactKeyboardEventI): Callback = {
      if (event.nativeEvent.keyCode == KeyCode.Enter ){
        event.preventDefault()
        closeDropdown
      } else if ( event.nativeEvent.keyCode == KeyCode.Escape){
        event.preventDefault()
        closeDropdown
      }
      else
        Callback()
    }

    def closeDropdown: Callback = $.modState(_.copy(isOpen = false))

    def onClick(P: Props)(e: ReactEventI): Callback ={
      e.preventDefault()
      val text: String = e.target.textContent.toString
      println(text)

      if(text == "Template 1")
        P.dispatch(SetTemplate1) >> closeDropdown
      else if(text == "Template 2")
        P.dispatch(SetTemplate) >> closeDropdown
      else
        P.dispatch(NoAction) >> closeDropdown

//      text match {
//        case "Template 1" => println(e.target.value)
//          P.dispatch(SetTemplate1) >> closeDropdown
//        case "Template 2" => println(e.target.value)
//          P.dispatch(SetTemplate) >> closeDropdown
//        case s: String => println(s)
//          Callback()
//      }
    }
  }

  val component = ReactComponentB[Props]("TemplateSelect")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(dispatch: Action => Callback) = component.set()(Props(dispatch))
}
