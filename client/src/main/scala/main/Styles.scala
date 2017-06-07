package main

import scalacss.Defaults._
import scalacss.internal.mutable.GlobalRegistry
import shared._


/**
  * Created by johan on 2/23/17.
  */

object Styles extends StyleSheet.Inline {

  import dsl._

  val searchView = style(
    overflow.auto,
    width(100.%%),
    height(40.%%)
  )

  val dragList = style(
    overflow.hidden,
    width(100.%%),
    height(46.%%)
  )
  val treeView = style(
    overflow.auto,
    width(100.%%),
    height(86.%%),
    float.left
  )

  val bootstrapButton = style(
    addClassName("btn")
  )

  val clearfix = style(

  )
  val navBarButton = style(
    addClassName("btn btn-default navbar-btn"),
    margin(5.px),
    padding(10.px)
  )

  val navBarDropdown = style(
    addClassName("btn btn-default dropdown-toggle"),
    padding(10.px)
  )

  val removeButtonSimple = style(
    addClassName("btn glyphicon glyphicon-remove pull-right")
  )

  val bootStrapRemoveButton = style(
    addClassName("btn glyphicon glyphicon-remove pull-right"),
    //    borderBottomRightRadius(0.px),
    //    borderTopRightRadius(0.px),
    position.absolute,
    width(40.px),
    height(100.%%),
    top(0.px),
    left(92.%%)
  )

  val bootStrapContentButton = style(
    addClassName("btn glyphicon glyphicon-align-right pull-right"),
    border(1.px),
    position.absolute,
    height(100.%%),
    left(60.%%)
  )

  val navBar = style(
    addClassName("navbar navbar-default navbar-static-bottom")
  )

  val select = style(
    addClassName("form-control pull-right"),
    position.absolute,
    width(100.px),
    top(0.%%),
    height(100.%%),
    left(65.%%),
    border(1.px)
  )

  /**
    * Dropdown
    */


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