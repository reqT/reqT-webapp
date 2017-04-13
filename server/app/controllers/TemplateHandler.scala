package controllers

import scala.io.Source

/**
  * Created by phiped on 4/13/17.
  */
class TemplateHandler {
  val filePathTemplates = "server/app/controllers/templates.txt"

  def getTemplate(message: String): String = {
    val contentIterator = Source.fromFile(filePathTemplates).getLines
    val templateNbr = message.drop(8).toInt

    val template = "Template" + templateNbr + "="
    var wanted = false

    def take(line: String): Boolean = {
      if (line.replace(" ", "").startsWith(template))
        wanted = true
      if (line.trim.isEmpty)
        wanted = false
      wanted
    }

    contentIterator.filter(take).mkString("\n")
  }


  def getAmountOfTemplates: Int = {
    var amount = 0
    Source.fromFile(filePathTemplates).getLines.foreach(line => if(line.startsWith("Template")) amount+=1)
    amount
  }

}
