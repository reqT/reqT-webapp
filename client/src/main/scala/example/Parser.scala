//package example
//
///**
//  * Created by johan on 5/5/17.
//  */
//object Parser {
//  val str = "Model(Stakeholder(\"Lovelace\") has (Feature(\"export\") has ( Benefit(4), Feature(\"Hejsan\")),Feature(\"print\") has Benefit(1), Feature(\"upload\") has Benefit(2)),Stakeholder(\"Babbage\") has ( Feature(\"export\") has Benefit(2),Feature(\"print\") has Benefit(1), Feature(\"upload\") has Benefit(1)))"
//
//  parseModel(str).toString
//
//  def parseModel(model: String): Model = {
//    var m = Model(Tree(Seq()))
//    var level = 0
//    var ind = model.indexOf("(") + 1
//
//    //Entities or Attributes
//    val entOrAttrMatch = """\w+[(]["]?\w+["]?[)]""".r
//    //Entities or StringAttributes
//    val entOrStrAttrmatch = """\w+[(]["]\w+["][)]""".r
//    //IntAttributes
//    val intAttrMatch = """\w+[(]\w+[)]""".r
//    //RelationType (has to be trimmed)
//    val relationTypeMatch = """\s\w+\s""".r
//
//    //Relation with children
//    val relationWithChildrenMatch = """\w+[(]["]\w+["][)](\s\w+\s)?[(]""".r
//
//    //
//    val relationWithoutChildrenMatch = """\w+[(]["]\w+["][)](\s\w+\s)?(\w+[(]["]\w+["][)])?\w+[(]\w+[)]""".r
//
//    while(ind < model.length){
//      var elem = entOrAttrMatch.findFirstIn(model) match {
//        case Some(found) =>
//          ind += found.length
//          found
//        case None => None
//      }
//
//      model.charAt(ind) match {
//        case ' ' =>
//          var tempType = relationTypeMatch.findFirstIn(model).get
//          ind += tempType.length
//          var relType = tempType.trim.asInstanceOf[RelationType]
//
//        case ',' =>
//      }
//
//
//    }
//
//  }
//
//  def parseSubmodel(submodel: String): Seq[Any] = {
//
//  }
//
//}
