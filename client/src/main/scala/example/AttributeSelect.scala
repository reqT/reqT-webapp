package example

import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent, _}
import diode.Action
import shared._

/**
  * Created by johan on 4/4/17.
  */


object AttributeSelect {

  def selectStyle() = Seq(
    ^.className := "form-control pull-right",
    ^.width := "155px",
    ^.height :=  "100%",
    ^.color := "#03EE7D",
    ^.background := "white",
    ^.textAlign.center,
    ^.textAlignLast.center
  )

  case class Props(value: String, isIntAttr: Boolean, setNewAttribute: Option[Attribute] => Callback)

  case class State(value: String)

  val intAttributeList = List("Benefit", "Capacity", "Cost", "Damage", "Frequency", "Min", "Max", "Order", "Prio", "Probability", "Profit", "Value")

  val stringAttributeList = List("Code", "Comment", "Deprecated", "Example", "Expectation", "FileName", "Gist", "Image", "Spec", "Text", "Title", "Why")
//
//  def intAttrfromString(value: String): Option[IntAttribute] = {
//    Vector("Benefit", "Capacity", "Cost", "Damage", "Frequency", "Min", "Max", "Order", "Prio", "Probability", "Profit", "Value").find(_ == value)
//  }
//
//  def stringAttrfromString(value: String): Option[StringAttribute] = {
//    Vector("Code", "Comment", "Deprecated", "Example", "Expectation", "FileName", "Gist", "Image", "Spec", "Text", "Title", "Why").find(_ == value)
//  }


  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      <.select(
        selectStyle(),
        ^.value := { if(S.value.isEmpty) P.value else S.value },
        ^.onChange ==> onChange(P, S)
      )(
        P.isIntAttr ?= intAttributeList.map(x => <.option(x)),
        !P.isIntAttr ?= stringAttributeList.map(x => <.option(x))
      )


    def onChange(P: Props, S: State)(e: ReactEventI): Callback = {
      val newAttr = e.target.value
      e.preventDefault()

      if (P.isIntAttr) {
        val attribute = Some(IntAttribute(newAttr))
        P.setNewAttribute(attribute) >> $.setState(s = S.copy(value = newAttr))
      } else {
        val attribute = Some(StringAttribute(newAttr))
        P.setNewAttribute(attribute) >> $.setState(s = S.copy(value = newAttr))
      }
    }
  }

  val component = ReactComponentB[Props]("RelationSelect")
    .initialState(State(value = ""))
    .renderBackend[Backend]
    .build



  def apply(value: String, isIntAttr: Boolean, setNewAttribute: Option[Attribute] => Callback)
  = component.set()(Props(value, isIntAttr, setNewAttribute))

}
