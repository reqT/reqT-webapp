package shared

/**
  * Configuration file for server host location.
  */

case class Config() {
  val hostname= "localhost"
  val port = "9000"
  val localhost = "ws://localhost:9000/socket"
  val socketURL = s"ws://$hostname:$port/socket"
}
