package example

import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent, _}
import diode.Action


/**
  * Created by johan on 4/4/17.
  */


object RelationSelect {

  def selectStyle = Seq(
    ^.className := "form-control pull-right",
    ^.width := "155px",
    ^.height := "100%",
    ^.color := "black",
    ^.borderBottomLeftRadius := "0px",
    ^.borderTopLeftRadius := "0px",
    ^.boxShadow := "5px 6px 12px 0px rgba(0,0,0,0.2)",
    ^.background := "#FFC2C2",
    ^.border := "0px"
  )

  case class Props(value: String, dispatch: Action => Callback, updateRel: Option[RelationType] => Action, isModelValue: Boolean)

  case class State(value: String)

  def fromString(value: String): Option[RelationType] = {
    Vector(binds, deprecates, excludes, has, helps, hurts, impacts, implements, interactsWith, is, precedes, requires, relatesTo, superOf, verifies).find(_.toString == value)
  }


  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      <.select(
        selectStyle,
        ^.value := P.value,
        ^.onChange ==> onChange(P, S)
      )(
        <.option("binds"),
        <.option("deprecates"),
        <.option("excludes"),
        <.option("has"),
        <.option("helps"),
        <.option("hurts"),
        <.option("impacts"),
        <.option("implements"),
        <.option("interactsWith"),
        <.option("is"),
        <.option("precedes"),
        <.option("requires"),
        <.option("relatesTo"),
        <.option("superOf"),
        <.option("verifies")
      )


    def onChange(props: Props, state: State)(e: ReactEventI): Callback = {
      e.preventDefault()
      props.dispatch(props.updateRel(fromString(e.target.value)))


    }


  }


  val component = ReactComponentB[Props]("RelationSelect")
    .initialState(State(value = ""))
    .renderBackend[Backend]
    .build



  def apply(value: String, dispatch: Action => Callback, updateRel: Option[RelationType] => Action, isModelValue: Boolean) = component.set()(Props(value, dispatch, updateRel, isModelValue))

}
