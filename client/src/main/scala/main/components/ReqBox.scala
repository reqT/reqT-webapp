package main.components

import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react.{BackendScope, ReactComponentB, _}

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import main.ReactTreeView.{ToggleCollapsed, AddTuple, Tuple}
import main.CollapseCircuit
import main.AppCircuit
import shared._

object ReqBox {

  object Style extends StyleSheet.Inline {

    import dsl._

    val headBox = style(
      display.flex,
      flexWrap.wrap,
      width(100.%%),
      height(500.px)
    )

    val outerRed = style(backgroundColor.darkred)
    val outerGreen = style(backgroundColor.darkgreen)
    val outerBlue = style(backgroundColor.darkblue)
    val outerOrange = style(backgroundColor.darkorange)

    val outerBox = style(
      display.flex,
      flexWrap.wrap,
      justifyContent.center,
      alignItems.center,
      width(50.%%),
      height(50.%%)
    )

    val innerRed = style(backgroundColor.red)
    val innerGreen = style(backgroundColor.green)
    val innerBlue = style(backgroundColor.blue)
    val innerOrange = style(backgroundColor.orange)

    val innerBox = style(
      display.flex,
      justifyContent.center,
      alignItems.center,
      color.white,
      fontSize(11.pt),
      width(45.%%),
      height(45.%%),
      cursor.pointer
    )
  }

  val sc = AppCircuit.connect(_.tree)
  val cc = CollapseCircuit.connect(_.list)

  val s1: List[String] = List("context", "intentions", "requirements", "delivery")
  val s2: List[List[String]] = List(
    List("stakeholders", "product", "systems", "interfaces"),
    List("goals", "priorities", "risks", "commitments"),
    List("functions", "data", "qualities", "tests"),
    List("roadmap", "resources", "constraints", "releasePlan")
  )

  def findElemUUID(es: Seq[Elem], name: String): Option[(UUID, Seq[Elem])] = {
    //println("search space:")

    val optElem = es.find({
      //case Entity(_, id) => println(s"$id == $name"); id == name
      case Relation(e, _, _) => /*println(s"${e.id} == $name");*/ e.id == name
      case _ => false
    })

    //println(s"search: $name -- found: ${optElem.get.uuid}")

    optElem.getOrElse(None) match {
      case Relation(e, _, s) => /*println(s"NOTEQ: ${e.uuid} != ${optElem.get.uuid}");*/ Some((e.uuid, s.children))
      //case Entity(_, uuid) => Some((uuid, Seq()))
      case _ => None
    }
  }

  def togglePath(s1: String, s2: String, tree: Tree, collapseProxy: ModelProxy[Seq[Tuple]]): TagMod =
    ^.onClick --> {
      val s1Elem = findElemUUID(tree.children, s1)

      val s2Elem = s1Elem.map({
        case (_, es) => findElemUUID(es, s2)
        case _ => None
      })

      s2Elem.flatten match {
        case Some((uuid, _)) => {
          //println(s"collapsing: $uuid")
          //collapseProxy.dispatchCB(AddTuple(Tuple(uuid, collapsed = false))) >>
          collapseProxy.dispatchCB(ToggleCollapsed(s1Elem.get._1)) >>
            collapseProxy.dispatchCB(ToggleCollapsed(uuid))
        }
        case _ => Callback()
      }
    }

  case class State()
  case class Props()

  class Backend($: BackendScope[Unit, State]) {
    def render(S: State) = {
      sc((treeProxy: ModelProxy[Tree]) => cc((collapseProxy: ModelProxy[Seq[Tuple]]) => {
        def onClick(s1: String, s2: String) =
          togglePath(s1, s2, treeProxy.value, collapseProxy)

        <.pre(
          Style.headBox,
          <.div(
            Style.outerBox + Style.outerRed,
            <.div(Style.innerBox + Style.innerRed, "Stakeholders", onClick(s1(0), s2(0)(0))),
            <.div(Style.innerBox + Style.innerGreen, "Product", onClick(s1(0), s2(0)(1))),
            <.div(Style.innerBox + Style.innerBlue, "Systems", onClick(s1(0), s2(0)(2))),
            <.div(Style.innerBox + Style.innerOrange, "Interfaces", onClick(s1(0), s2(0)(3)))
          ),
          <.div(
            Style.outerBox + Style.outerGreen,
            <.div(Style.innerBox + Style.innerRed, "Goals", onClick(s1(1), s2(1)(0))),
            <.div(Style.innerBox + Style.innerGreen, "Priorities", onClick(s1(1), s2(1)(1))),
            <.div(Style.innerBox + Style.innerBlue, "Risks", onClick(s1(1), s2(1)(2))),
            <.div(Style.innerBox + Style.innerOrange, "Commitments", onClick(s1(1), s2(1)(3)))
          ),
          <.div(
            Style.outerBox + Style.outerBlue,
            <.div(Style.innerBox + Style.innerRed, "Functions", onClick(s1(2), s2(2)(0))),
            <.div(Style.innerBox + Style.innerGreen, "Data", onClick(s1(2), s2(2)(1))),
            <.div(Style.innerBox + Style.innerBlue, "Qualities", onClick(s1(2), s2(2)(2))),
            <.div(Style.innerBox + Style.innerOrange, "Tests", onClick(s1(2), s2(2)(3)))
          ),
          <.div(
            Style.outerBox + Style.outerOrange,
            <.div(Style.innerBox + Style.innerRed, "Road-map", onClick(s1(3), s2(3)(0))),
            <.div(Style.innerBox + Style.innerGreen, "Resources", onClick(s1(3), s2(3)(1))),
            <.div(Style.innerBox + Style.innerBlue, "Constraints", onClick(s1(3), s2(3)(2))),
            <.div(Style.innerBox + Style.innerOrange, "Release plan", onClick(s1(3), s2(3)(3)))
          )
        )
      }))
    }
  }

  val component = ReactComponentB[Unit]("ElementList")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply() = component.set()()
}
