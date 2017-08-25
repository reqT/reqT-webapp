package selects

import diode.Action
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, _}
import org.scalajs.dom.document
import shared._



object RelationSelect {

  def selectStyle(P: Props) = Seq(
    ^.className := "form-control pull-right",
    ^.width := "155px",
    ^.height := {
      if (P.isModelValue) "100%" else ""
    },
    ^.color := {
      if (P.isModelValue) "black" else "#FF3636"
    },
    ^.borderBottomLeftRadius := {
      if (P.isModelValue) "0px" else "5px"
    },
    ^.borderTopLeftRadius := {
      if (P.isModelValue) "0px" else "5px"
    },
    ^.boxShadow := "5px 6px 12px 0px rgba(0,0,0,0.2)",
    ^.background := {
      if (P.isModelValue) "#FFC2C2" else "white"
    },
    ^.textAlign.center,
    ^.textAlignLast.center
  )

  case class Props(value: String, dispatch: Option[Action => Callback], updateRel: Option[Option[RelationType] => Action], isModelValue: Boolean, setNewRelation: Option[Option[RelationType] => Callback], saveScrollPosition: Option[Double => Callback])

  case class State(value: String)

  val relationList = List("binds", "deprecates", "excludes", "has", "helps", "hurts", "impacts", "implements", "interactsWith", "is",
    "precedes", "requires", "relatesTo", "superOf", "verifies")

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      <.select(
        selectStyle(P),
        ^.value := {
          if (S.value.isEmpty) P.value else S.value
        },
        ^.onChange ==> onChange(P, S)
      )(
        relationList.map(x => <.option(x))
      )

    def saveScrollPos(P: Props): Callback = {
      P.saveScrollPosition match {
        case Some(saveScrollPosition) =>
          saveScrollPosition(document.getElementById("treeView").scrollTop)
        case None =>
          Callback()
      }
    }

    def onChange(P: Props, S: State)(e: ReactEventI): Callback = {
      e.preventDefault()

      val newRel = e.target.value

      if (P.isModelValue)
        P.dispatch match {
          case Some(dispatch) => dispatch(P.updateRel.get(Some(RelationType(newRel)))) >> saveScrollPos(P)
          case None => Callback(println("Error: Missing dispatch"))
        }
      else
        P.setNewRelation match {
          case Some(setRelation) => setRelation(Some(RelationType(newRel))) >> $.setState(s = S.copy(value = e.target.value))
          case None => Callback(println("Error: Missing setNewRelation method"))
        }
    }


  }


  val component = ReactComponentB[Props]("RelationSelect")
    .initialState(State(value = ""))
    .renderBackend[Backend]
    .build


  def apply(value: String, dispatch: Option[Action => Callback], updateRel: Option[Option[RelationType] => Action], isModelValue: Boolean, setNewRelation: Option[Option[RelationType] => Callback], saveScrollPosition: Option[Double => Callback])
  = component.set()(Props(value, dispatch, updateRel, isModelValue, setNewRelation, saveScrollPosition))

}
