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
    ^.position := "absolute",
    ^.width := "100px",
    ^.top := "0%",
    ^.height := "100%",
    ^.left := "65%",
    ^.background := "#FFC2C2"
  )

  case class Props(value: String, dispatch: Action => Callback, updateRel: Option[RelationType] => Action)

  def fromString(value: String): Option[RelationType] = {
    Vector(binds, deprecates, excludes, has, helps, hurts, impacts, implements, interactsWith, is, precedes, requires, relatesTo, superOf, verifies).find(_.toString == value)
  }

  def onChange(props: Props)(e: ReactEventI): Callback ={
    e.preventDefault()

    var link = "example." ++ props.value
//    println(fromString(e.target.value))
    props.dispatch(props.updateRel(fromString(e.target.value)))
  }



  class Backend(t: BackendScope[Props, _]) {
    def render(P: Props) =
      <.select(
        selectStyle,
        ^.value := P.value,
        ^.onChange ==> onChange(P)
      )(
        <.option("has"),
        <.option("requires"),
        <.option("precedes")
      )

  }

  val component = ReactComponentB[Props]("Modal")
    .stateless //initialState(State(isOpen = false))
    .renderBackend[Backend]
    .build



  def apply(value: String, dispatch: Action => Callback, updateRel: Option[RelationType] => Action) = component.set()(Props(value, dispatch, updateRel))

}
