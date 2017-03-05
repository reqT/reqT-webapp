package example

import diode._
import diode.react.ReactConnector
import diode.ActionResult.ModelUpdate

import scala.annotation.tailrec

object AppCircuit extends Circuit[Model] with ReactConnector[Model] {


  def initialModel: Model = Model(Tree(Seq(Req("R1"), Req("R2"), Stakeholder("BOSS"),
    Relation(Req("R3"), has, Tree(Seq(Relation(Req("R3.1"), has, Tree(Seq(Prio(1))))))), Relation(Req("R4"), has, Tree(Seq(Prio(2)))))))

  def zoomToChildren(modelRW: ModelRW[Model, Tree], path: Seq[String]): Option[ModelRW[Model, Seq[Elem]]] = {
    if (path.isEmpty) {
      Some(modelRW.zoomTo(_.children))
    } else {
      val currentElem = path.head
      val index = modelRW.value.children.indexWhere(retrieveEntity(_).toString == currentElem)
      val elemFound = retrieveEntity(modelRW.value.children(index))

      if (elemFound.hasRelation) {
        println("hasRelation")
        val elemRW = modelRW.zoomRW(_.children.apply(index))((tree, newElem) => tree.copy(children = (tree.children.take(index) :+ newElem) ++ tree.children.drop(index + 1)))
        zoomToChildren(elemRW.zoomRW(_.asInstanceOf[Relation].submodel)((relation, newSubmodel) => relation.asInstanceOf[Relation].copy(submodel = newSubmodel)), path.tail)

      } else if (elemFound.isEntity) {
        println("isEntity")
        zoomToChildren(modelRW, path.tail)
      } else {
        println("Can't add Elems to Attribute")
        None
      }
    }
  }

  def retrieveEntity(elem: Elem): Elem = elem match {
    case relation: Relation => relation.entity
    case node: Node => node
  }


  class TreeHandler[M](modelRW: ModelRW[Model, Tree]) extends ActionHandler(modelRW) {
    override def handle = {
      case Reset => updated(Tree(Seq()))

      case AddElem(path: Seq[String], newElem: Elem) => {
        zoomToChildren(modelRW, path.tail) match {
          case Some(modelRW) => {
            modelRW.value.find(_.toString == path.last) match {
              case Some(foundElem) => updated(modelRW.updated(modelRW.value.map(elem => if (elem == foundElem) Relation(elem.asInstanceOf[Entity], has, Tree(Seq(newElem))) else elem)).tree)

              case None => updated(modelRW.updated(modelRW.value :+ newElem).tree)
            }
          }
          case None => noChange
        }
      }
      case RemoveElem(path: Seq[String]) => {
        noChange
      }

    }


  }

  override protected def actionHandler = new TreeHandler(zoomTo(_.tree))
}