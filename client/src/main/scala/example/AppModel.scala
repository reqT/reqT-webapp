package example

import diode.Action

case class Model(tree: Tree)

case class Tree(children: Seq[Elem])

sealed trait Elem {
  var isRelation = false
  var hasRelation = false
  var isEntity = false
  var isAttribute = false
  var isStringAttribute = false
  var isIntAttribute = false

}


case class Relation(entity: Entity, var link: RelationType, submodel:Tree) extends Elem {
  isRelation = true
  entity.hasRelation = true

  def setLink(newLink: RelationType): Relation = {
    link = newLink
    this
  }

  def setEntity(newEntityName: String): Relation = {
    entity.setID(newEntityName)
    this
  }

}

sealed trait Node extends Elem

sealed trait Attribute[T] extends Node {
  def value: T
  isAttribute = true
}

sealed trait Entity extends Node {
  var id: String
  def setID(newID:String): Node = {
    id = newID
    this
  }
  def getID() : String = id
  isEntity = true

}

sealed trait RelationType

//sealed trait AttributeTypes[T]
sealed trait General extends Entity

sealed trait Context extends Entity

sealed trait Requirement extends Entity

sealed trait DataReq extends Requirement

sealed trait DesignReq extends Requirement

sealed trait FunctionalReq extends Requirement

sealed trait GeneralReq extends Requirement

sealed trait QualityReq extends Requirement

sealed trait ScenarioReq extends Requirement

sealed trait VariabilityReq extends Requirement

sealed trait StringAttribute extends Attribute[String]{
  isStringAttribute = true
  var value:String
  def setValue(newValue:String): StringAttribute = {
    value = newValue
    this
  }
}

sealed trait IntAttribute extends Attribute[Int]{
  isIntAttribute = true
  var value: Int
  def setValue(newValue:Int): IntAttribute = {
    value = newValue
    this
  }
}

sealed trait VectorAttribute[T] extends Attribute[Vector[T]]


sealed trait StatusValueAttribute extends Attribute[Enumeration]

/**
  * General Entities
  */
case class Item(var id: String) extends General

case class Label(var id: String) extends General

case class Meta(var id: String) extends General

case class Section(var id: String) extends General

case class Term(var id: String) extends General

/**
  * Context Entities
  */
case class Actor(var id: String) extends Context

case class App(var id: String) extends Context

case class Component(var id: String) extends Context

case class Domain(var id: String) extends Context

case class Module(var id: String) extends Context

case class Product(var id: String) extends Context

case class Release(var id: String) extends Context

case class Resource(var id: String) extends Context

case class Risk(var id: String) extends Context

case class Service(var id: String) extends Context

case class Stakeholder(var id: String = "") extends Context

case class System(var id: String) extends Context

case class User(var id: String) extends Context

/**
  * Data Requirements
  */

case class Class(var id: String) extends DataReq

case class Data(var id: String) extends DataReq

case class Input(var id: String) extends DataReq

case class Member(var id: String) extends DataReq

case class Output(var id: String) extends DataReq

case class Relationship(var id: String) extends DataReq

/**
  * Design Requirements
  */

case class Design(var id: String) extends DesignReq

case class Screen(var id: String) extends DesignReq

case class MockUp(var id: String) extends DesignReq

/**
  * Functional Requirements
  */

case class Function(var id: String) extends FunctionalReq

case class Interface(var id: String) extends FunctionalReq

/**
  * General Requirements
  */

case class Epic(var id: String) extends GeneralReq

case class Feature(var id: String) extends GeneralReq

case class Goal(var id: String) extends GeneralReq

case class Idea(var id: String) extends GeneralReq

case class Issue(var id: String) extends GeneralReq

case class Req(var id: String) extends GeneralReq

case class Ticket(var id: String) extends GeneralReq

case class WorkPackage(var id: String) extends GeneralReq

/**
  * Quality Requirements
  */

case class Breakpoint(var id: String) extends QualityReq

case class Barrier(var id: String) extends QualityReq

case class Quality(var id: String) extends QualityReq

case class Target(var id: String) extends QualityReq

/**
  * Scenario Requirements
  */
case class Scenario(var id: String) extends ScenarioReq

case class Task(var id: String) extends ScenarioReq

case class Test(var id: String) extends ScenarioReq

case class Story(var id: String) extends ScenarioReq

case class UseCase(var id: String) extends ScenarioReq

/**
  * Variability Requirements
  */
case class VariationPoint(var id: String) extends VariabilityReq

case class Variant(var id: String) extends VariabilityReq

/**
  * String Attribute Types
  */
case class Code(var value: String) extends StringAttribute

case class Comment(var value: String) extends StringAttribute

case class Deprecated(var value: String) extends StringAttribute

case class Example(var value: String) extends StringAttribute

case class Expectation(var value: String) extends StringAttribute

case class FileName(var value: String) extends StringAttribute

case class Gist(var value: String) extends StringAttribute

case class Image(var value: String) extends StringAttribute

case class Spec(var value: String) extends StringAttribute

case class Text(var value: String) extends StringAttribute

case class Title(var value: String) extends StringAttribute

case class Why(var value: String) extends StringAttribute

/**
  * Int Attribute Types
  */

case class Benefit(var value: Int) extends IntAttribute

case class Capacity(var value: Int) extends IntAttribute

case class Cost(var value: Int) extends IntAttribute

case class Damage(var value: Int) extends IntAttribute

case class Frequency(var value: Int) extends IntAttribute

case class Min(var value: Int) extends IntAttribute

case class Max(var value: Int) extends IntAttribute

case class Order(var value: Int) extends IntAttribute

case class Prio(var value: Int) extends IntAttribute

case class Probability(var value: Int) extends IntAttribute

case class Profit(var value: Int) extends IntAttribute

case class Value(var value: Int) extends IntAttribute

/**
  * StatusValue Attribute Types
  */

object Status extends Enumeration {
  type Status = Value
  val ELICITED, SPECIFIED, VALIDATED, PLANNED, IMPLEMENTED, TESTED, RELEASED, FAILED, POSTPONED, DROPPED = Value
}

/**
  * Vector Attribute Types
  */

case class Constraints[T](value: Vector[T]) extends VectorAttribute[T]

/**
  * Relation Types - case objects or case class? objects because singleton seems ok
  */

case object binds extends RelationType

case object deprecates extends RelationType

case object excludes extends RelationType

case object has extends RelationType

case object helps extends RelationType

case object hurts extends RelationType

case object impacts extends RelationType

case object implements extends RelationType

case object interactsWith extends RelationType

case object is extends RelationType

case object precedes extends RelationType

case object requires extends RelationType

case object relatesTo extends RelationType

case object superOf extends RelationType

case object verifies extends RelationType


/**
  * Actions
  */
case object Init extends Action

case object AddE extends Action

case class AddElem(path: Seq[String], elem: Elem, relationType: RelationType) extends Action

case class RemoveElem(path: Seq[String]) extends Action

case class MoveElem(oldPath: Seq[String], newPath: Seq[String], relationType: RelationType) extends Action

case class updateEntity(path: Seq[String], newId: String) extends  Action

case class updateStringAttribute(path: Seq[String], newValue: String) extends  Action

case class updateIntAttribute(path: Seq[String], newValue: Int) extends  Action

case class updateRelation(path: Seq[String], newId: String, newRelationType: Option[RelationType]) extends  Action

case object SetTemplate extends Action

case class Select() extends Action

case object Reset extends Action

case object Test extends Action

case object NoAction extends Action



