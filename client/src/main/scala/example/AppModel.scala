package example

import diode.Action

import scala.util.Random.nextInt

case class Model(tree: Tree)

case class Tree(children: Seq[Elem]){ override def toString: String = children.map(_.toString).toString.replace("List", "Model")}

case class UUID(x1: Int, x2: Int, x3: Int, x4: Int)

object UUID {
  var modeluuid = random()

  def random(): UUID = {
    import scala.util.Random.nextInt
    new UUID(
      nextInt(),
      nextInt() & 0xffff0fff | 0x00004000,
      nextInt() & 0x3f000000 | 0x80000000,
      nextInt())
  }

  def model(): UUID = modeluuid
}

sealed trait Elem {
  var isRelation = false
  var hasRelation = false
  var isEntity = false
  var isAttribute = false
  var isStringAttribute = false
  var isIntAttribute = false

  var uuid: UUID

  def setRandomUUID() = {
    uuid = UUID.random()
  }

  def setUUID(newUUID: UUID) = {
    uuid = newUUID
  }

  def getWithRelation(boolean: Boolean) : Elem= {
    hasRelation = boolean
    this
  }
}


case class Relation(var entity: Entity, var link: RelationType, submodel:Tree) extends Elem {
  isRelation = true
  entity.hasRelation = true
  var uuid : UUID = UUID.random()

  override def toString: String = entity.toString + " " + link.toString + " " + submodel.toString.replaceAll("List", "")


  def setLink(newLink: RelationType): Relation = {
    link = newLink
    this
  }

  def setEntity(newEntityName: String): Relation = {
    entity.setID(newEntityName)
    this
  }

  def setEntity(newEntity: Entity): Relation = {
    entity = newEntity
    this
  }

}

sealed trait Node extends Elem {

}


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
  def getID: String = id

  override def toString: String = (getClass.getName + "(\"" + id + "\")").replace("example.", "")

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

  override def toString: String = (getClass.getName + "(\"" + value + "\")").replace("example.", "")

}

sealed trait IntAttribute extends Attribute[Int]{
  isIntAttribute = true
  var value: Int
  def setValue(newValue:Int): IntAttribute = {
    value = newValue
    this
  }

  override def toString: String = (getClass.getName + "(" + value + ")").replace("example.", "")

}

sealed trait VectorAttribute[T] extends Attribute[Vector[T]]


sealed trait StatusValueAttribute extends Attribute[Enumeration]

/**
  * General Entities
  */
case class Item(var id: String= "", var uuid: UUID = UUID.random()) extends General {
}

case class Label(var id: String= "", var uuid: UUID = UUID.random()) extends General {
}

case class Meta(var id: String= "", var uuid: UUID = UUID.random()) extends General {
}

case class Section(var id: String= "", var uuid: UUID = UUID.random()) extends General

case class Term(var id: String= "", var uuid: UUID = UUID.random()) extends General

/**
  * Context Entities
  */
case class Actor(var id: String= "", var uuid: UUID = UUID.random()) extends Context

case class App(var id: String= "", var uuid: UUID = UUID.random()) extends Context

case class Component(var id: String= "", var uuid: UUID = UUID.random()) extends Context

case class Domain(var id: String= "", var uuid: UUID = UUID.random()) extends Context

case class Module(var id: String= "", var uuid: UUID = UUID.random()) extends Context

case class Product(var id: String= "", var uuid: UUID = UUID.random()) extends Context

case class Release(var id: String= "", var uuid: UUID = UUID.random()) extends Context

case class Resource(var id: String= "", var uuid: UUID = UUID.random()) extends Context

case class Risk(var id: String= "", var uuid: UUID = UUID.random()) extends Context

case class Service(var id: String= "", var uuid: UUID = UUID.random()) extends Context

case class Stakeholder(var id: String = "", var uuid: UUID = UUID.random()) extends Context

case class System(var id: String= "", var uuid: UUID = UUID.random()) extends Context

case class User(var id: String= "", var uuid: UUID = UUID.random()) extends Context

/**
  * Data Requirements
  */

case class Class(var id: String= "", var uuid: UUID = UUID.random()) extends DataReq

case class Data(var id: String= "", var uuid: UUID = UUID.random()) extends DataReq

case class Input(var id: String= "", var uuid: UUID = UUID.random()) extends DataReq

case class Member(var id: String= "", var uuid: UUID = UUID.random()) extends DataReq

case class Output(var id: String= "", var uuid: UUID = UUID.random()) extends DataReq

case class Relationship(var id: String= "", var uuid: UUID = UUID.random()) extends DataReq

/**
  * Design Requirements
  */

case class Design(var id: String= "", var uuid: UUID = UUID.random()) extends DesignReq

case class Screen(var id: String= "", var uuid: UUID = UUID.random()) extends DesignReq

case class MockUp(var id: String= "", var uuid: UUID = UUID.random()) extends DesignReq

/**
  * Functional Requirements
  */

case class Function(var id: String= "", var uuid: UUID = UUID.random()) extends FunctionalReq

case class Interface(var id: String= "", var uuid: UUID = UUID.random()) extends FunctionalReq

/**
  * General Requirements
  */

case class Epic(var id: String= "", var uuid: UUID = UUID.random()) extends GeneralReq

case class Feature(var id: String= "", var uuid: UUID = UUID.random()) extends GeneralReq

case class Goal(var id: String= "", var uuid: UUID = UUID.random()) extends GeneralReq

case class Idea(var id: String= "", var uuid: UUID = UUID.random()) extends GeneralReq

case class Issue(var id: String= "", var uuid: UUID = UUID.random()) extends GeneralReq

case class Req(var id: String= "", var uuid: UUID = UUID.random()) extends GeneralReq

case class Ticket(var id: String= "", var uuid: UUID = UUID.random()) extends GeneralReq

case class WorkPackage(var id: String= "", var uuid: UUID = UUID.random()) extends GeneralReq

/**
  * Quality Requirements
  */

case class Breakpoint(var id: String= "", var uuid: UUID = UUID.random()) extends QualityReq

case class Barrier(var id: String= "", var uuid: UUID = UUID.random()) extends QualityReq

case class Quality(var id: String= "", var uuid: UUID = UUID.random()) extends QualityReq

case class Target(var id: String= "", var uuid: UUID = UUID.random()) extends QualityReq

/**
  * Scenario Requirements
  */
case class Scenario(var id: String= "", var uuid: UUID = UUID.random()) extends ScenarioReq

case class Task(var id: String= "", var uuid: UUID = UUID.random()) extends ScenarioReq

case class Test(var id: String= "", var uuid: UUID = UUID.random()) extends ScenarioReq

case class Story(var id: String= "", var uuid: UUID = UUID.random()) extends ScenarioReq

case class UseCase(var id: String= "", var uuid: UUID = UUID.random()) extends ScenarioReq

/**
  * Variability Requirements
  */
case class VariationPoint(var id: String= "", var uuid: UUID = UUID.random()) extends VariabilityReq

case class Variant(var id: String= "", var uuid: UUID = UUID.random()) extends VariabilityReq

/**
  * String Attribute Types
  */
case class Code(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

case class Comment(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

case class Deprecated(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

case class Example(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

case class Expectation(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

case class FileName(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

case class Gist(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

case class Image(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

case class Spec(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

case class Text(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

case class Title(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

case class Why(var value: String= "", var uuid: UUID = UUID.random()) extends StringAttribute

/**
  * Int Attribute Types
  */

case class Benefit(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

case class Capacity(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

case class Cost(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

case class Damage(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

case class Frequency(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

case class Min(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

case class Max(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

case class Order(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

case class Prio(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

case class Probability(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

case class Profit(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

case class Value(var value: Int=0, var uuid: UUID = UUID.random()) extends IntAttribute

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

case class Constraints[T](value: Vector[T], var uuid: UUID = UUID.random()) extends VectorAttribute[T]

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

case class AddElem(path: Seq[String], elem: Elem, relationType: RelationType) extends Action

case class RemoveElem(path: Seq[String]) extends Action

case class RemoveEmptyRelation(path: Seq[String]) extends Action

case class MoveElem(oldPath: Seq[String], newPath: Seq[String], relationType: RelationType) extends Action

case class UpdateEntity(path: Seq[String], newEntity: Entity) extends Action

case class UpdateIntAttribute(path: Seq[String], newStringAttribute: IntAttribute) extends  Action

case class UpdateStringAttribute(path: Seq[String], newIntAttribute: StringAttribute) extends  Action

case class UpdateRelation(path: Seq[String], newId: String, newRelationType: Option[RelationType]) extends  Action

case class UpdateEntireRelation(path: Seq[String], newEntity: Entity, newRelationType: Option[RelationType]) extends Action

case class SetTemplate(tree : Tree) extends  Action

case object SetTemplate extends Action

case object SetTemplate1 extends Action

case object NoAction extends Action



