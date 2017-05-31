
package example

import diode._
import diode.react.ReactConnector
import example.ReactTreeView.{AddTuple, RemoveTuple, ToggleCollapsed, Tuple}
import shared._

object CollapseCircuit extends Circuit[Seq[Tuple]] with ReactConnector[Seq[Tuple]] {

  def initialModel: Seq[Tuple] = Seq[Tuple]()

  class CollapseListHandler[M](modelRW: ModelRW[M, Seq[Tuple]]) extends ActionHandler(modelRW) {
    override def handle = {
      case AddTuple(tuple: Tuple) =>
        updated(modelRW.value :+ tuple)
      case RemoveTuple(uuid: UUID) =>
        updated(modelRW.value.filter(_.uuid != uuid))
      case ToggleCollapsed(uuid: UUID) =>
        updated(modelRW.value.map(x => if(x.uuid == uuid) {
          x.copy(collapsed = !x.collapsed)
          x
        } else x))
    }
    }


  override protected def actionHandler = new CollapseListHandler(zoomTo(_.seq))
}
