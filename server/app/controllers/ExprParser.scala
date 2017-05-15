package controllers

import shared._
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by johan on 5/12/17.
  */
class ExprParser extends RegexParsers {

  val int = "[0-9][0-9]*".r
  val string = "[a-zA-Z0-9~!@#$^%&*|;:'{}\\[\\] /<,>.+?`-][a-zA-Z0-9~!@#$^%&*|;:'{}\\[\\] /<,>.+?`-]*".r
  val relType = "[a-zA-Z][a-zA-Z]*".r

  def Model: Parser[shared.Model] = "Model(" ~ opt(rep(Elem ~ ",")) ~ Elem ~")"^^{
    case   _ ~ Some(list) ~ elem ~ _=> {
      new Model(Tree(list.map(_._1) ++ Seq(elem.asInstanceOf[shared.Elem])))
    }
  }

  def Elem: Parser[shared.Elem] = Relation | Node

  def Relation: Parser[shared.Relation] = Entity ~ RelationType ~ RelationModel ^^{
    case ent ~ reltype ~ relationModel => new Relation(ent,reltype, relationModel)
  }

  def RelationModel : Parser[shared.Tree] = RelationList | Elem  ^^ {
    case list: shared.Tree => list
    case elem: shared.Elem => Tree(Seq(elem))
  }

  def RelationList: Parser[shared.Tree] = "(" ~ rep1(Elem ~ ",") ~ Elem ~ ")" ^^{
    case _ ~ list ~ elem ~ _ => Tree(list.map(_._1) ++ Seq(elem.asInstanceOf[shared.Elem]))
  }

  def RelationType: Parser[shared.RelationType] = relType ^^{new RelationType(_)}

  def Node: Parser[shared.Node] = (Attribute | Entity) ^^{
    case node :Node => node
  }

  def Attribute: Parser[shared.Attribute] = IntAttribute //| StringAttribute

  def IntAttribute: Parser[shared.IntAttribute] = string ~ "(" ~ opt(int) ~ ")" ^^{
    case tpe ~ _ ~ Some(value) ~ _ => new IntAttribute(tpe,value.toInt)
  }

  //    def StringAttribute: Parser[StringAttribute] = string ~ "(\"" ~ opt(string) ~ "\")" ^^{
  //      case tpe ~ _ ~ Some(value) ~ _ => new StringAttribute(tpe,value)
  //    }

  def Entity: Parser[shared.Entity] = string ~ "(" ~ "\"" ~ opt(string) ~ "\"" ~ ")" ^^{
    case tpe ~ _ ~  _ ~ Some(id) ~ _ ~ _ => new Entity(tpe,id)
    case tpe ~ _ ~  _ ~ None ~ _ ~ _ => new Entity(tpe)
  }
}
