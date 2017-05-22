package controllers

import scala.io.Source

/**
  * Created by phiped on 4/13/17.
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

//    if(templateNbr > getAmountOfTemplates)
//      None
//    else
      Some(contentIterator.filter(take).mkString("\n"))
  }


  def getAmountOfTemplates: Int = {
    var amount = 0
    Source.fromFile(filePathTemplates).getLines.foreach(line => if(line.replace(" ", "").startsWith("Template=")) amount+=1)
    amount
  }

}
