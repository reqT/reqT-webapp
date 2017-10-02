package controllers

import shared._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class ExprParser extends RegexParsers {
  val int: Regex = "[0-9][0-9]*".r
  val string: Regex = "[a-zA-Z0-9~!@#$^%&*|;:'{}\\[\\]\\(\\) /_<,>.+?`-][a-zA-Z0-9~!@#$^%&*|;:'{}\\[\\]\\(\\) /_<,>.+?`-]*".r
  val reqType: Regex = "[a-zA-Z][a-zA-Z]*".r
  val lpar: Regex = " *\\( *".r
  val rpar: Regex = " *\\) *".r
  val entityType: Regex = "(Ent|Meta|Item|Label|Section|Term|Actor|App|Component|Domain|Module|Product|Release|Resource|Risk|Service|Stakeholder|System|User|Class|Data|Input|Member|Output|Relationship|Design|Screen|MockUp|Function|Interface|State|Event|Epic|Feature|Goal|Idea|Issue|Req|Ticket|WorkPackage|Breakpoint|Barrier|Quality|Target|Scenario|Task|Test|Story|UseCase|VariationPoint|Variant)".r
  val intAttrType: Regex = "(Benefit|Capacity|Cost|Damage|Frequency|Min|Max|Order|Prio|Probability|Profit|Value)".r
  val stringAttrType: Regex = "(Comment|Deprecated|Example|Expectation|FileName|Gist|Image|Spec|Text|Title|Why)".r
  val statusValue: Regex = "(ELICITED|SPECIFIED|VALIDATED|PLANNED|IMPLEMENTED|TESTED|RELEASED|FAILED|POSTPONED|DROPPED)".r

  def Model: Parser[shared.Model] = "Model(" ~ opt(opt(rep(Elem ~ ",")) ~ Elem) ~ ")" ^^ {
    case _ ~ Some(Some(list) ~ elem) ~ _ => shared.Model(Tree(list.map(_._1) ++ Seq(elem.asInstanceOf[shared.Elem])))
    case none => shared.Model(Tree(Seq()))
  }

  def Elem: Parser[shared.Elem] = Relation | Node

  def Relation: Parser[shared.Relation] = Entity ~ RelationType ~ RelationModel ^^ {
    case ent ~ reltype ~ relationModel => shared.Relation(ent, reltype, relationModel)
  }

  def RelationModel: Parser[shared.Tree] = RelationList | Elem ^^ {
    case list: shared.Tree => list
    case elem: shared.Elem => Tree(Seq(elem))
  }

  def RelationList: Parser[shared.Tree] = lpar ~ opt(rep(Elem ~ ",") ~ Elem) ~ rpar ^^ {
    case _ ~ Some(list ~ elem) ~ _ => Tree(list.map(_._1) ++ Seq(elem.asInstanceOf[shared.Elem]))
    case _ ~ none ~ _ => Tree(Seq())
  }

  def RelationType: Parser[shared.RelationType] = reqType ^^ {
    shared.RelationType
  }

  def Node: Parser[shared.Node] = (Attribute | Entity) ^^ {
    case node: Node => node
  }

  def Attribute: Parser[shared.Attribute] = IntAttribute | StringAttribute | StatusValueAttribute

  def IntAttribute: Parser[shared.IntAttribute] = intAttrType ~ lpar ~ opt(int) ~ rpar ^^ {
    case tpe ~ _ ~ Some(value) ~ _ => shared.IntAttribute(tpe, value.toInt)
  }

  def StringAttribute: Parser[shared.StringAttribute] = stringAttrType ~ lpar ~ "\"" ~ opt(string) ~ "\"" ~ rpar ^^ {
    case tpe ~ _ ~ _ ~ Some(id) ~ _ ~ _ => shared.StringAttribute(tpe, id)
    case tpe ~ _ ~ _ ~ None ~ _ ~ _ => shared.StringAttribute(tpe)
  }

  def StatusValueAttribute: Parser[shared.StatusValueAttribute] = "Status" ~ lpar ~ statusValue ~ rpar ^^{
      case _ ~ value ~ _ => shared.StatusValueAttribute(value = value) // ' "Status" ~ ' i början
  }

  def Entity: Parser[shared.Entity] = entityType ~ lpar ~ "\"" ~ opt(string) ~ "\"" ~ rpar ^^ {
    case tpe ~ _ ~ _ ~ Some(id) ~ _ ~ _ => shared.Entity(tpe, id)
    case tpe ~ _ ~ _ ~ None ~ _ ~ _ => shared.Entity(tpe)
  }

}
