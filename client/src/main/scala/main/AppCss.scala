package main


import scalacss.Defaults._
import scalacss.internal.mutable.GlobalRegistry


object AppCss {

  def load = {
    GlobalRegistry.register(
      GlobalStyle,
      ElementList.Style
    )

    GlobalRegistry.onRegistration(_.addToDocument())
  }

}
