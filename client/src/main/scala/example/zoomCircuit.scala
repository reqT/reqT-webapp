package example

import diode._
import diode.react.ReactConnector
import example.ReactTreeView.{ZoomIn, ZoomOut}

case class Zoom(factor: Double)

object ZoomCircuit extends Circuit[Zoom] with ReactConnector[Zoom] {


  def initialModel: Double = 1

  class CollapseListHandler[M](modelRW: ModelRW[M, Double]) extends ActionHandler(modelRW) {
    override def handle = {
      case ZoomIn() =>
        if(modelRW.value < 1)
          updated(modelRW.value + 0.05)
        else
          updated(modelRW.value)
      case ZoomOut() =>
        if(modelRW.value > 0.3)
          updated(modelRW.value - 0.05)
        else
          updated(modelRW.value)
    }
  }

  override protected def actionHandler = new CollapseListHandler(zoomTo(_.factor))
}
