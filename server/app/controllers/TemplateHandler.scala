package controllers

import scala.io.Source


/**
  * Currently not in use.
  * Could be used for loading templates from the server.
  */

class TemplateHandler {
  val filePathTemplates = "server/app/controllers/templates.txt"
  val nbrOfTemplates = getAmountOfTemplates

  def getTemplate(templateNbr: Int): Option[String] = {
    val contentIterator = Source.fromFile(filePathTemplates).getLines

    val template = "Template" + templateNbr + "="
    var wanted = false

    def take(line: String): Boolean = {
      if (line.replace(" ", "").startsWith(template))
        wanted = true
      if (line.trim.isEmpty)
        wanted = false
      wanted
    }

    Some(contentIterator.filter(take).mkString("\n"))
  }


  def getAmountOfTemplates: Int = {
    var amount = 0
    Source.fromFile(filePathTemplates).getLines.foreach(line => if (line.replace(" ", "").startsWith("Template=")) amount += 1)
    amount
  }

}
