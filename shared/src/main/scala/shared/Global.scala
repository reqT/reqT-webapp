package shared

/**
  * Created by phiped on 8/21/17.
  */

case class Global() {
  val hostname= "vm45.cs.lth.se"
  val port = "9000"
  val socketURL = s"ws://$hostname:$port/socket"

  def getSocketURL = socketURL
}
