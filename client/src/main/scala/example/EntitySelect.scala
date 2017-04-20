package example

import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent, _}
import diode.Action


/**
  * Created by johan on 4/4/17.
  */


object EntitySelect {

  val entityList = List("Item", "Label", "Meta", "Section", "Term", "Actor", "App", "Component", "Domain", "Module", "Product", "Release",
  "Risk", "Service", "Stakeholder", "System", "User", "Class", "Data", "Input", "Member", "Output", "Relationship", "Design", "Screen", "MockUp",
  "Function", "Interface", "Epic", "Feature", "Goal", "Idea", "Issue", "Req", "Ticket", "WorkPackage", "Breakpoint", "Barrier", "Quality", "Target",
  "Function", "Interface", "Scenario", "Task", "Test", "Story", "UseCase", "VariantPoint", "Variant")

  def selectStyle = Seq(
    ^.className := "form-control pull-right",
    ^.width := "155px",
    ^.height := "100%",
    ^.color := "black",
    ^.borderBottomLeftRadius := "0px",
    ^.borderTopLeftRadius := "0px",
    ^.boxShadow := "5px 6px 12px 0px rgba(0,0,0,0.2)",
    ^.background := "#cedbe7",
    ^.border := "0px",
    ^.textAlign.center,
    ^.textAlignLast.center
  )

  case class Props(value: String, dispatch: Action => Callback, updateEntity: Option[Entity => Action] , isModelValue: Boolean)

  case class State(value: String)

  def fromString(value: String): Option[Entity] = {
    Vector(Item(), Label(), Meta(), Section(), Term(), Actor(), App(), Component(), Domain(), Module(), Product(), Release(),
      Risk(), Service(), Stakeholder(), System(), User(), Class(), Data(), Input(), Member(), Output(), Relationship(), Design(), Screen(), MockUp(),
      Function(), Interface(), Epic(), Feature(), Goal(), Idea(), Issue(), Req(), Ticket(), WorkPackage(), Breakpoint(), Barrier(), Quality(), Target(),
      Scenario(), Task(), Test(), Story(), UseCase(), VariationPoint(), Variant()).find(_.toString == value)
  }


  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      <.select(
        selectStyle,
        ^.value := {
          if (S.value.isEmpty) P.value else S.value
        },
        ^.onChange ==> onChange(P, S)
      )(
        entityList.map(x => <.option(x))
      )


    def onChange(P: Props, S: State)(e: ReactEventI): Callback = {
      e.preventDefault()
      $.setState(s = S.copy(value = e.target.value))
    }

  }



  val component = ReactComponentB[Props]("EntitySelect")
    .initialState(State(value = ""))
    .renderBackend[Backend]
    .build



  def apply(value: String, dispatch: Action => Callback, updateEntity: Option[Entity => Action], isModelValue: Boolean) = component.set()(Props(value, dispatch, updateEntity, isModelValue))

}
