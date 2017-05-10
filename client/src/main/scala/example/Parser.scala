package example

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.reflect.annotation.EnableReflectiveInstantiation

/**
  * Created by johan on 5/5/17.
  */
object Parser {
  val str = "Model(Stakeholder(\"Lovelace\") has (Feature(\"export\") has ( Benefit(4), Feature(\"Hejsan\")),Feature(\"print\") has Benefit(1), Feature(\"upload\") has Benefit(2)),Stakeholder(\"Babbage\") has ( Feature(\"export\") has Benefit(2),Feature(\"print\") has Benefit(1), Feature(\"upload\") has Benefit(1)))"
  //Entities or Attributes
  val entOrAttrMatch = """\w+[(]["]?\w+["]?[)]""".r
  //Entities or StringAttributes
  val entOrStrAttrMatch = """\w+[(]["]\w+["][)]""".r
  //IntAttributes
  val intAttrMatch = """\w+[(]\w+[)]""".r
  //RelationType (has to be trimmed)
  val relationTypeMatch = """\s\w+\s""".r

  //Relation with children
  val relationWithChildrenMatch = """\w+[(]["]\w+["][)](\s\w+\s)?[(]""".r

  // Relation without children
  val relationWithoutChildrenMatch = """\w+[(]["]\w+["][)](\s\w+\s)?(\w+[(]["]\w+["][)])?\w+[(]\w+[)]""".r

  parseModel(str).toString


  @JSExport
  def newInstanceFromString(className: String)(args: Any*): Any = {
    val ctor = className.split("\\.").foldLeft(js.Dynamic.global) { (prev, part) =>
      prev.selectDynamic(part)
    }
    js.Dynamic.newInstance(ctor)(args.asInstanceOf[Seq[js.Any]]: _*)
  }

  def parseModel(model: String): Model = {
    var m = Model(Tree(Seq()))
    var level = 0
    var ind = model.indexOf("(") + 1



    while(ind < model.length){
      var elem = entOrAttrMatch.findFirstIn(model) match {
        case Some(found) =>
          ind += found.length
          found
        case None => None
      }
      model.charAt(ind) match {
        case ' ' =>
          var tempType = relationTypeMatch.findFirstIn(model).get
          ind += tempType.length
          val relType = newInstanceFromString("Req")(js.Any.fromString("1"))
          println(tempType)
//          println(relType)
          parseSubmodel(model.substring(ind, model.length))

        case ',' =>



      }


    }
    m
  }

  def parseSubmodel(submodel: String): Seq[Any] = {
    println(submodel)

    Seq()
  }

}
