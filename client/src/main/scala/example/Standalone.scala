package example

import scalacss.defaults.Exports.StyleSheet
import scalacss.Defaults._
/**
  * Created by johan on 2017-03-06.
  */
object Standalone extends StyleSheet.Standalone {
  import dsl._

  "html" - {
    height(100.%%)
  }

  "body" - {
    height(100.%%)
  }
}