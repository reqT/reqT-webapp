package example


import scalacss.Defaults._
/**
  * Created by johan on 2017-03-06.
  */
object Standalone extends StyleSheet.Standalone {
  import dsl._

  "html" - {
    height(100.vh)
    backgroundColor(lavender)
  }

  "body" - {
    height(100.vh)
  }
  "div.content" - {
    height(100.vh)
    backgroundColor(lavender)
  }
}