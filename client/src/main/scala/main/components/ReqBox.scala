package main.components

import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react.{ReactComponentB, _}

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import main.ReactTreeView.Tuple
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

  def findElemUUID(es: Seq[Elem], name: String): Option[(UUID, Seq[Elem])] = {
    val optElem = es.find({
      case Entity(_, id) => id == name
      case Relation(e, _, _) => e.id == name
      case _ => false
    })

    optElem.getOrElse(None) match {
      case Relation(e, _, s) => Some((e.uuid, s.children))
      case e: Entity => Some((e.uuid, Seq()))
      case _ => None
    }
  }

  def getUUID(path: (String, String), tree: Tree, collapseProxy: ModelProxy[Seq[Tuple]]): (String, String, Option[Seq[UUID]]) = {
    val s1Elem = findElemUUID(tree.children, path._1)

    val s2Elem = s1Elem.map({
      case (_, es) => findElemUUID(es, path._2)
      case _ => None
    })

    s2Elem.flatten match {
      case Some((uuid, _)) => (path._1, path._2, Some(Seq(s1Elem.get._1, uuid)))
      case _ => (path._1, path._2, None)
    }
  }

  def mapSectionWithSections(x: String, xs: List[String]): List[(String, String)] =
    if (xs.nonEmpty)
      List((x, xs.head)) ++ mapSectionWithSections(x, xs.tail)
    else
      List()

  def createSections(xs: List[String], xss: List[List[String]]): List[(String, String)] =
    if (xs.nonEmpty && xss.nonEmpty)
      mapSectionWithSections(xs.head, xss.head) ++ createSections(xs.tail, xss.tail)
    else
      List()

  def constructOnClick(uuid1: UUID, uuid2: UUID,
                       collapseProxy: ModelProxy[Seq[Tuple]],
                       collapseAllCB: Callback): TagMod = {
    ^.onClick --> {
      collapseAllCB >>
        collapseProxy.dispatchCB(SetCollapse(uuid1, collapsed = false)) >>
        collapseProxy.dispatchCB(SetCollapse(uuid2, collapsed = false))
    }
  }

  def collapseAllSections(uuids: List[Option[Seq[UUID]]],
                          collapseProxy: ModelProxy[Seq[Tuple]]): Callback = {
    uuids.map({
      case Some(Seq(uuid1, uuid2)) =>
        collapseProxy.dispatchCB(SetCollapse(uuid1, collapsed = true)) >>
          collapseProxy.dispatchCB(SetCollapse(uuid2, collapsed = true))
      case _ => Callback()
    }).fold(Callback.empty)((a, b) => a >> b)
  }

  case class SetCollapse(uuid: UUID, collapsed: Boolean) extends Action

  val s1: List[String] = List("context", "intentions", "requirements", "delivery")
  val s2: List[List[String]] = List(
    List("stakeholders", "product", "systems", "interfaces"),
    List("goals", "priorities", "risks", "commitments"),
    List("functions", "data", "qualities", "tests"),
    List("roadmap", "resources", "constraints", "releasePlan")
  )

  lazy val sections = createSections(s1, s2)

  val sc = AppCircuit.connect(_.tree)
  val cc = CollapseCircuit.connect(_.list)

  val component = ReactComponentB[Unit]("ElementList")
    .render($ =>
      sc((treeProxy: ModelProxy[Tree]) => cc((collapseProxy: ModelProxy[Seq[Tuple]]) => {
        val uuidPaths: List[(String, String, Option[Seq[UUID]])] =
          sections.map(x => getUUID(x, treeProxy.value, collapseProxy))

        val collapseAllCb = collapseAllSections(uuidPaths.map(x => x._3), collapseProxy)

        val onClicks: List[Seq[TagMod]] =
          uuidPaths.map({
            case (_, _, Some(Seq(uuid1, uuid2))) =>
              Seq(constructOnClick(uuid1, uuid2, collapseProxy, collapseAllCb))
            case _ =>
              Seq(
                ^.onClick --> Callback(),
                ^.opacity := "0.3",
                ^.disabled := "true",
                ^.cursor := "default"
              )
          })

        <.pre(
          Style.headBox,
          <.div(
            Style.outerBox + Style.outerRed,
            <.div(Style.innerBox + Style.innerRed, "Stakeholders", onClicks(0)),
            <.div(Style.innerBox + Style.innerGreen, "Product", onClicks(1)),
            <.div(Style.innerBox + Style.innerBlue, "Systems", onClicks(2)),
            <.div(Style.innerBox + Style.innerOrange, "Interfaces", onClicks(3))
          ),
          <.div(
            Style.outerBox + Style.outerGreen,
            <.div(Style.innerBox + Style.innerRed, "Goals", onClicks(4)),
            <.div(Style.innerBox + Style.innerGreen, "Priorities", onClicks(5)),
            <.div(Style.innerBox + Style.innerBlue, "Risks", onClicks(6)),
            <.div(Style.innerBox + Style.innerOrange, "Commitments", onClicks(7))
          ),
          <.div(
            Style.outerBox + Style.outerBlue,
            <.div(Style.innerBox + Style.innerRed, "Functions", onClicks(8)),
            <.div(Style.innerBox + Style.innerGreen, "Data", onClicks(9)),
            <.div(Style.innerBox + Style.innerBlue, "Qualities", onClicks(10)),
            <.div(Style.innerBox + Style.innerOrange, "Tests", onClicks(11))
          ),
          <.div(
            Style.outerBox + Style.outerOrange,
            <.div(Style.innerBox + Style.innerRed, "Road-map", onClicks(12)),
            <.div(Style.innerBox + Style.innerGreen, "Resources", onClicks(13)),
            <.div(Style.innerBox + Style.innerBlue, "Constraints", onClicks(14)),
            <.div(Style.innerBox + Style.innerOrange, "Release plan", onClicks(15))
          )
        )
      }))
    )
    .build

  def apply() = component.set()()
}
