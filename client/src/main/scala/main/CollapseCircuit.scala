package main

import diode._
import diode.react.ReactConnector
import main.ReactTreeView.{AddTuple, RemoveTuple, ToggleCollapsed, Tuple}
import shared._

case class ColModel(list: Seq[Tuple])

object CollapseCircuit extends Circuit[ColModel] with ReactConnector[ColModel] {


  def initialModel: ColModel = ColModel(Seq[Tuple]())

  class CollapseListHandler[M](modelRW: ModelRW[M, Seq[Tuple]]) extends ActionHandler(modelRW) {
    override def handle = {
      case AddTuple(tuple: Tuple) =>
        modelRW.value.find(_.uuid == tuple.uuid) match {
          case Some(tuple) =>
            updated(modelRW.value)
          case None =>
            updated(modelRW.value :+ tuple)
        }

      case RemoveTuple(uuid: UUID) =>
        updated(modelRW.value.filter(_.uuid != uuid))

      case ToggleCollapsed(uuid: UUID) =>

        updated(modelRW.value.map(elem => if (elem.uuid == uuid) {
          Tuple(elem.uuid, !elem.collapsed)
        } else elem))
    }
  }

  override protected def actionHandler = new CollapseListHandler(zoomTo(_.list))
}
