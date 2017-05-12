package controllers

import scala.io.Source


class ElementListHandler {
  val filePathEntities = "server/app/controllers/entities.txt"
  val filePathIntAttributes = "server/app/controllers/intattributes.txt"
  val filePathStringAttributes = "server/app/controllers/stringattributes.txt"
  val filePathStatusValueAttributes = "server/app/controllers/statusvalueattributes.txt"

  def getEntities: List[String] = {
    val contentIterator = Source.fromFile(filePathEntities).getLines
    val entities = contentIterator.toList
    entities
  }

  def getIntAttributes: List[String] = {
    val contentIterator = Source.fromFile(filePathIntAttributes).getLines
    val intAttributes = contentIterator.toList
    intAttributes
  }

  def getStringAttributes: List[String] = {
    val contentIterator = Source.fromFile(filePathStringAttributes).getLines
    val stringAttributes = contentIterator.toList
    stringAttributes
  }

  def getStatusValueAttributes: List[String] = {
    val contentIterator = Source.fromFile(filePathStatusValueAttributes).getLines
    val statusValueAttributes = contentIterator.toList
    statusValueAttributes
  }

}
