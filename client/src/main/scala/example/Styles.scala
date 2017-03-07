package example


import scalacss.Defaults._


/**
  * Created by johan on 2/23/17.
  */

object Styles extends StyleSheet.Inline {
  import dsl._

  val searchView = style(
    overflow.auto,
    width(100.%%),
    height(49.%%)
//    float.left

  )

  val dragList = style(
    overflow.auto,
    width(100.%%),
    height(50.%%)
//    float.left


  )
  val treeView = style(
    overflow.auto,
    width(80.%%),
    height(100.%%),
    float.left
  )


  val listElem = style(

  )

  val bootstrapButton = style(
    addClassName("btn")
  )

  val clearfix = style(

  )
  val navBarButton = style(
    addClassName("btn btn-default navbar-btn")
  )

  val navBarDropdown = style(
    addClassName("btn btn-default dropdown-toggle")

  )

  val bootStrapRemoveButton = style(
    addClassName("btn glyphicon glyphicon-remove pull-right"),
    border(1.px),
//    border.solid,
    position.absolute,
    height(100.%%),
    left(90.%%)
  )

  val bootStrapContentButton = style(
    addClassName("btn glyphicon glyphicon-align-right pull-right"),
    border(1.px),
//    border.solid,
    position.absolute,
    height(100.%%),
    left(80.%%)
  )

  val navBar = style(
    addClassName("navbar navbar-default navbar-static-bottom")
  )

  /**
    * Styles not in use yet, not validated
    */
  val selectedTreeItemContent = style(
    backgroundColor(c"#1B8EB0"),
    color.white,
    fontWeight._400,
    padding(v = 0.px, h = 7.px)
  )

  val treeItemBefore = style(
    display.inlineBlock,
    fontSize(11.px),
    color.grey,
    marginTop(3.px),
    marginRight(7.px),
    marginBottom(0.px),
    marginLeft(0.px)
  )


}