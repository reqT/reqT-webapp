package shared

//
case class ActorMessage[T](value : T)
//
//case class Template[Int](override val value: Int) extends ActorMessage
//
//case class Message[String](override val value: String) extends ActorMessage