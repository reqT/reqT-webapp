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

    val darkRed = style(backgroundColor(c"#a40000"))
    val darkGreen = style(backgroundColor(c"#4e9a06"))
    val darkBlue = style(backgroundColor(c"#204a87"))
    val darkOrange = style(backgroundColor(c"#ce5c00"))

    val outerBox = style(
      display.flex,
      flexWrap.wrap,
      justifyContent.center,
      alignItems.center,
      width(50.%%),
      height(50.%%),
      border(1.px, solid, white)
    )

    val lightRed = style(backgroundColor(c"#cc0000"))
    val lightGreen = style(backgroundColor(c"#73d216"))
    val lightBlue = style(backgroundColor(c"#2b62b5"))
    val lightOrange = style(backgroundColor(c"#f57900"))

    val innerBox = style(
      display.flex,
      justifyContent.center,
      alignItems.center,
      color.white,
      fontSize(11.pt),
      width(45.%%),
      height(45.%%),
      cursor.pointer,
      border(1.px, solid, white)
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
            Style.outerBox + Style.darkRed,
            <.div(Style.innerBox + Style.lightRed, "Stakeholders", onClicks(0)),
            <.div(Style.innerBox + Style.darkGreen, "Product", onClicks(1)),
            <.div(Style.innerBox + Style.darkBlue, "Systems", onClicks(2)),
            <.div(Style.innerBox + Style.darkOrange, "Interfaces", onClicks(3))
          ),
          <.div(
            Style.outerBox + Style.darkGreen,
            <.div(Style.innerBox + Style.darkRed, "Goals", onClicks(4)),
            <.div(Style.innerBox + Style.lightGreen, "Priorities", onClicks(5)),
            <.div(Style.innerBox + Style.darkBlue, "Risks", onClicks(6)),
            <.div(Style.innerBox + Style.darkOrange, "Commitments", onClicks(7))
          ),
          <.div(
            Style.outerBox + Style.darkBlue,
            <.div(Style.innerBox + Style.darkRed, "Functions", onClicks(8)),
            <.div(Style.innerBox + Style.darkGreen, "Data", onClicks(9)),
            <.div(Style.innerBox + Style.lightBlue, "Qualities", onClicks(10)),
            <.div(Style.innerBox + Style.darkOrange, "Tests", onClicks(11))
          ),
          <.div(
            Style.outerBox + Style.darkOrange,
            <.div(Style.innerBox + Style.darkRed, "Road-map", onClicks(12)),
            <.div(Style.innerBox + Style.darkGreen, "Resources", onClicks(13)),
            <.div(Style.innerBox + Style.darkBlue, "Constraints", onClicks(14)),
            <.div(Style.innerBox + Style.lightOrange, "Release plan", onClicks(15))
          )
        )
      }))
    )
    .build

  def apply() = component.set()()
}
