package main

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^.{^, _}

import scala.scalajs.js
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import shared._

object ReactSearchBox {

  class Style extends StyleSheet.Inline {

    import dsl._

    val searchBox = style(marginBottom(10 px))

    val input = style(
      addClassName("form-control"),
      fontSize(13 px),
      fontWeight._300,
      padding(3 px),
      backgroundColor.transparent,
      borderBottom :=! "1px solid #B2ADAD"
    )
  }

  case class Props(onTextChange: String => Callback, style: Style)

  class Backend(t: BackendScope[Props, _]) {
    def onTextChange(P: Props)(e: ReactEventI) =
      e.preventDefaultCB >> P.onTextChange(e.target.value)

    def render(P: Props) =
      <.div(P.style.searchBox)(
        <.input(
          P.style.input,
          ^.width := "20%",
          ^.maxWidth := "200px",
          ^.placeholder := "Search",
          ^.onKeyUp ==> onTextChange(P)
        )
      )
  }

  object DefaultStyle extends Style

  val component = ReactComponentB[Props]("ReactSearchBox")
    .stateless
    .renderBackend[Backend]
    .build


  def apply(onTextChange: String => Callback, style: Style = DefaultStyle, ref: js.UndefOr[String] = "", key: js.Any = {}) = component.set(key, ref)(Props(onTextChange, style))

}


