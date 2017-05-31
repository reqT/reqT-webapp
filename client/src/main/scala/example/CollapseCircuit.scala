
package example

import diode._
import diode.react.ReactConnector
import example.ReactTreeView.{AddTuple, RemoveTuple, ToggleCollapsed, Tuple}
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

        updated(modelRW.value.map(x => if(x.uuid == uuid) {
          Tuple(x.uuid, !x.collapsed)
        } else x))
    }
    }

  override protected def actionHandler = new CollapseListHandler(zoomTo(_.list))
}
