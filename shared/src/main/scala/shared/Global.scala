package shared

/**
  * Created by phiped on 8/21/17.
  */

case class Global() {
  val hostname= "vm45.cs.lth.se"
  val port = "9000"
  val localhost = "ws://localhost:9000/socket"
  val socketURL = s"ws://$hostname:$port/socket"
}
