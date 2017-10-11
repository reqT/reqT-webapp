package main


import scalacss.Defaults._
import scalacss.internal.mutable.GlobalRegistry

import components.ReqBox
import components.SideViewTopHeader

object AppCss {

  def load = {
    GlobalRegistry.register(
      GlobalStyle,
      ElementList.Style,
      ReqBox.Style,
      SideViewTopHeader.Style
    )

    GlobalRegistry.onRegistration(_.addToDocument())
  }

}
