package example

import diode.Action
import org.scalajs.dom.svg.Path

/**
  * Created by johan on 2/22/17.
  */

case class Model(tree: Tree)

case class Tree(children: Seq[Elem])

sealed trait Elem {
  var isRelation = false
  var hasRelation = false
  var isEntity = false
  var isAttribute = false

}


case class Relation(entity:Entity, link:RelationTypes, submodel:Tree) extends Elem {
  isRelation = true
  entity.hasRelation = true

}

sealed trait Node extends Elem

sealed trait Attribute[T] extends Node {
  val value: T
  isAttribute = true
}

sealed trait Entity extends Node {
  val id: String
  isEntity = true
}

sealed trait RelationTypes

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

sealed trait StringAttribute extends Attribute[String]

sealed trait IntAttribute extends Attribute[Int]

sealed trait VectorAttribute[T] extends Attribute[Vector[T]]

//sealed trait StatusValueAttribute extends Attribute[String] - What is this?

/**
  * General Entities
  */
case class Item(id: String) extends General

case class Label(id: String) extends General

case class Meta(id: String) extends General

case class Section(id: String) extends General

case class Term(id: String) extends General

/**
  * Context Entities
  */
case class Actor(id: String) extends Context

case class App(id: String) extends Context

case class Component(id: String) extends Context

case class Domain(id: String) extends Context

case class Module(id: String) extends Context

case class Product(id: String) extends Context

case class Release(id: String) extends Context

case class Resource(id: String) extends Context

case class Risk(id: String) extends Context

case class Service(id: String) extends Context

case class Stakeholder(id: String) extends Context

case class System(id: String) extends Context

case class User(id: String) extends Context

/**
  * Data Requirements
  */

case class Class(id: String) extends DataReq

case class Data(id: String) extends DataReq

case class Input(id: String) extends DataReq

case class Member(id: String) extends DataReq

case class Output(id: String) extends DataReq

case class Relationship(id: String) extends DataReq

/**
  * Design Requirements
  */

case class Design(id: String) extends DesignReq

case class Screen(id: String) extends DesignReq

case class MockUp(id: String) extends DesignReq

/**
  * Functional Requirements
  */

case class Function(id: String) extends FunctionalReq

case class Interface(id: String) extends FunctionalReq

/**
  * General Requirements
  */

case class Epic(id: String) extends GeneralReq

case class Feature(id: String) extends GeneralReq

case class Goal(id: String) extends GeneralReq

case class Idea(id: String) extends GeneralReq

case class Issue(id: String) extends GeneralReq

case class Req(id: String) extends GeneralReq

case class Ticket(id: String) extends GeneralReq

case class WorkPackage(id: String) extends GeneralReq

/**
  * Quality Requirements
  */

case class Breakpoint(id: String) extends QualityReq

case class Barrier(id: String) extends QualityReq

case class Quality(id: String) extends QualityReq

case class Target(id: String) extends QualityReq

/**
  * Scenario Requirements
  */
case class Scenario(id: String) extends ScenarioReq

case class Task(id: String) extends ScenarioReq

case class Test(id: String) extends ScenarioReq

case class Story(id: String) extends ScenarioReq

case class UseCase(id: String) extends ScenarioReq

/**
  * Variability Requirements
  */
case class VariationPoint(id: String) extends VariabilityReq

case class Variant(id: String) extends VariabilityReq

/**
  * String Attribute Types
  */
case class Code(value: String) extends StringAttribute

case class Comment(value: String) extends StringAttribute

case class Deprecated(value: String) extends StringAttribute

case class Example(value: String) extends StringAttribute

case class Expectation(value: String) extends StringAttribute

case class FileName(value: String) extends StringAttribute

case class Gist(value: String) extends StringAttribute

case class Image(value: String) extends StringAttribute

case class Spec(value: String) extends StringAttribute

case class Text(value: String) extends StringAttribute

case class Title(value: String) extends StringAttribute

case class Why(value: String) extends StringAttribute

/**
  * Int Attribute Types
  */

case class Benefit(value: Int) extends IntAttribute

case class Capacity(value: Int) extends IntAttribute

case class Cost(value: Int) extends IntAttribute

case class Damage(value: Int) extends IntAttribute

case class Frequency(value: Int) extends IntAttribute

case class Min(value: Int) extends IntAttribute

case class Max(value: Int) extends IntAttribute

case class Order(value: Int) extends IntAttribute

case class Prio(value: Int) extends IntAttribute

case class Probability(value: Int) extends IntAttribute

case class Profit(value: Int) extends IntAttribute

case class Value(value: Int) extends IntAttribute

/**
  * StatusValue Attribute Types
  */

//  case class Status(value: ??) extends StatusValueAttribute

/**
  * Vector Attribute Types
  */

case class Constraints[T](value: Vector[T]) extends VectorAttribute[T]

/**
  * Relation Types - case objects or case class? objects because singleton seems ok
  */

case object binds extends RelationTypes

case object deprecates extends RelationTypes

case object excludes extends RelationTypes

case object has extends RelationTypes

case object helps extends RelationTypes

case object hurts extends RelationTypes

case object impacts extends RelationTypes

case object implements extends RelationTypes

case object interactsWith extends RelationTypes

case object is extends RelationTypes

case object precedes extends RelationTypes

case object requires extends RelationTypes

case object relatesTo extends RelationTypes

case object superOf extends RelationTypes

case object verifies extends RelationTypes


/**
  * Actions
  */
case object Init extends Action

case object AddE extends Action

case class AddElem(path: Seq[String], elem: Elem) extends Action

case class RemoveElem(path: Seq[String]) extends Action

case class MoveElem(oldPath: Seq[String], newPath: Seq[String], elem: Elem) extends Action

case class UpdateElem(path: Seq[String], elem: Elem) extends  Action

case class Select() extends Action

case object Reset extends Action

case object Test extends Action

case object NoAction extends Action



