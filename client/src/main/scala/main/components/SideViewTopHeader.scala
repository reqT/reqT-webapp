package main.components

import scalacss.ScalaCssReact._
import scalacss.Defaults._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react._
import diode.react.ModelProxy

object SideViewTopHeader {

  object Style extends StyleSheet.Inline {
    import dsl._

    val viewModeLabel = style(
      display.inlineBlock,
      marginRight(4.px)
    )

    val delimiter = style(
      display.inlineBlock,
      width(6.px)
    )

    val editingModeButton = style(
      minWidth(70.px),
      minHeight(35.px)
    )

    val editingMode = style(
      marginLeft(10.px),
      display.inlineBlock,
      padding(4.px),
      backgroundColor(c"#f5f5f5"),
      borderTop(1.px, solid, c"rgb(204, 204, 204)"),
      borderRight(1.px, solid, c"rgb(204, 204, 204)"),
      borderLeft(1.px, solid, c"rgb(204, 204, 204)"),
      borderTopLeftRadius(4.px),
      borderTopRightRadius(4.px)
    )
  }

  object Buttons extends Enumeration {
    val EntityList, ReqBox = Value
  }

  def editingModeButton(name: String, isActive: Boolean,
                        setButton: Callback, followUpCb: Callback = Callback()) =
    Seq(
      <.button(
        ^.className := "btn" ++ (if (isActive) " btn-primary" else " btn-default"),
        ^.onClick --> (setButton >> followUpCb),
        Style.editingModeButton,
        name)
    )

  case class State(activeButton: Buttons.Value)

  case class Props(setEntityListView: Callback, setReqBoxView: Callback)

  val editingModeHeader = ReactComponentB[(Props, State)]("editingModeHeader")
    .initialState(State(activeButton = Buttons.EntityList))
    .render($ =>
      <.div(
      Style.editingMode,
      editingModeButton(
        "Entity List",
        isActive = $.state.activeButton == Buttons.EntityList,
        $.modState(_.copy(activeButton = Buttons.EntityList)),
        $.props._1.setEntityListView
      ),
      <.div(Style.delimiter), // Delimiter "invisible block"
      editingModeButton(
        "ReqBox",
        isActive = $.state.activeButton == Buttons.ReqBox,
        $.modState(_.copy(activeButton = Buttons.ReqBox)),
        $.props._1.setReqBoxView
      )
    )
    ).build

  def apply(setEntityListView: Callback, setReqBoxView: Callback) =
    editingModeHeader.set()((Props(setEntityListView, setReqBoxView), State(Buttons.EntityList)))
}
