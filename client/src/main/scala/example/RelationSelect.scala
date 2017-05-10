package example

import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent, _}
import diode.Action
import shared._


/**
  * Created by johan on 4/4/17.
  */


object RelationSelect {

  def selectStyle(P: Props) = Seq(
    ^.className := "form-control pull-right",
    ^.width := "155px",
    ^.height := {if(P.isModelValue) "100%" else ""},
    ^.color := {if(P.isModelValue) "black" else "#FF3636"},
    ^.borderBottomLeftRadius := {if(P.isModelValue) "0px" else "5px"},
    ^.borderTopLeftRadius := {if(P.isModelValue) "0px" else "5px"},
    ^.boxShadow := "5px 6px 12px 0px rgba(0,0,0,0.2)",
    ^.background := {if(P.isModelValue) "#FFC2C2" else "white"},
    ^.textAlign.center,
    ^.textAlignLast.center
  )

  case class Props(value: String, dispatch: Action => Callback, updateRel: Option[Option[RelationType] => Action], isModelValue: Boolean, setNewRelation: Option[Option[RelationType] => Callback])

  case class State(value: String)

  val relationList = List("binds", "deprecates", "excludes", "has", "helps", "hurts", "impacts", "implements", "interactsWith", "is",
  "precedes", "requires", "relatesTo", "superOf", "verifies")

  def fromString(value: String): Option[RelationType] = {
    Vector(binds, deprecates, excludes, has, helps, hurts, impacts, implements, interactsWith, is, precedes, requires, relatesTo, superOf, verifies).find(_.toString == value)
  }


  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      <.select(
        selectStyle(P),
        ^.value := { if(S.value.isEmpty) P.value else S.value },
        ^.onChange ==> onChange(P, S)
      )(
        relationList.map(x => <.option(x))
      )


    def onChange(P: Props, S: State)(e: ReactEventI): Callback = {
      e.preventDefault()

      if(P.isModelValue)
        P.dispatch(P.updateRel.get(fromString(e.target.value)))
      else
        P.setNewRelation match {
          case Some(setRelation) => setRelation(fromString(e.target.value)) >> $.setState(s = S.copy(value = e.target.value))
          case None => Callback(println("missing setNewRelation method"))
        }
    }


  }


  val component = ReactComponentB[Props]("RelationSelect")
    .initialState(State(value = ""))
    .renderBackend[Backend]
    .build



  def apply(value: String, dispatch: Action => Callback, updateRel: Option[Option[RelationType] => Action], isModelValue: Boolean, setNewRelation: Option[Option[RelationType] => Callback])
  = component.set()(Props(value, dispatch, updateRel, isModelValue, setNewRelation))

}
