package selects

import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, _}
import shared._


/**
  * Created by johan on 4/4/17.
  */


object EntitySelect {

  val entityList = List("Item", "Label", "Meta", "Section", "Term", "Actor", "App", "Component", "Domain", "Module", "Product", "Release",
  "Risk", "Service", "Stakeholder", "System", "User", "Class", "Data", "Input", "Member", "Output", "Relationship", "Design", "Screen", "MockUp",
  "Function", "Interface", "Epic", "Feature", "Goal", "Idea", "Issue", "Req", "Ticket", "WorkPackage", "Breakpoint", "Barrier", "Quality", "Target",
  "Function", "Interface", "Scenario", "Task", "Test", "Story", "UseCase", "VariantPoint", "Variant")

  def selectStyle(P: Props) = Seq(
    ^.className := "form-control pull-right",
    ^.width := "155px",
    ^.color := {if(P.isModelValue) "black" else "#047BEA"},
    ^.borderBottomLeftRadius := "5px",
    ^.borderTopLeftRadius := "5px",
    ^.background := {if(P.isModelValue) "#cedbe7" else "white"},
    ^.textAlign.center,
    ^.textAlignLast.center
  )

  case class Props(value: String, setNewEntity: Option[Entity] => Callback, isModelValue: Boolean)

  case class State(value: String)


  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      <.select(
        selectStyle(P),
        ^.value := {
          if (S.value.isEmpty) P.value else S.value
        },
        ^.onChange ==> onChange(P, S)
      )(
        entityList.map(x => <.option(^.font :="bold",x))
      )


    def onChange(P: Props, S: State)(e: ReactEventI): Callback = {
      e.preventDefault()
      val newEntity = e.target.value
      P.setNewEntity(Some(Entity(newEntity))) >> $.setState(s = S.copy(value = newEntity))
    }
  }


  val component = ReactComponentB[Props]("EntitySelect")
    .initialState(State(value = ""))
    .renderBackend[Backend]
    .build



  def apply(value: String, setNewEntity: Option[Entity] => Callback, isModelValue: Boolean) = component.set()(Props(value, setNewEntity, isModelValue))

}
