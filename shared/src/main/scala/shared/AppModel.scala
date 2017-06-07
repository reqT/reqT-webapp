package shared

import diode.Action


case class Model(tree: Tree) {

}

case class Tree(children: Seq[Elem]) {
  override def toString: String = {
    if (children.size == 1 && !children.head.isRelation)
      children.head.toString
    else {
      children.map(_.toString).toString.replace("List", "").replace("Vector", "")
    }
  }

  def makeString: String = {
    if (this.toString.startsWith("(") && this.toString.endsWith(")"))
      "Model" + this.toString
    else
      "Model(" + this.toString + ")"
  }
}

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
  var isStatusValueAttribute = false

  var uuid: UUID = UUID.random()

  def setRandomUUID() = {
    uuid = UUID.random()
  }

  def setUUID(newUUID: UUID) = {
    uuid = newUUID
  }

  def getWithRelation(boolean: Boolean): Elem = {
    hasRelation = boolean
    this
  }
}

sealed trait Node extends Elem

case class Relation(var entity: Entity, var link: RelationType, submodel: Tree) extends Elem {
  isRelation = true
  entity.hasRelation = true


  def modelString = "Relation(" + entity.modelString + "," + link.modelString + "," + submodel.toString + ")"

  override def toString: String = entity.toString + " " + link.toString + {
    if (submodel.children.length > 1) "\n" + submodel.toString else " " + submodel.toString
  }

  def setLink(newLink: RelationType): Relation = {
    link = newLink
    this
  }

  def setEntityID(newID: String): Relation = {
    entity.setID(newID)
    this
  }

  def setEntity(newEntity: Entity): Relation = {
    entity = newEntity
    this
  }
}

case class Entity(var entityType: String, var id: String = "") extends Node {
  isEntity = true

  def setID(newID: String): Node = {
    id = newID
    this
  }

  def setType(newType: String): Node = {
    entityType = newType
    this
  }

  def getID: String = id

  def getType: String = entityType

  def modelString = "Entity(\"" + entityType + "\",\"" + id + "\")"

  override def toString(): String = entityType + "(\"" + id + "\")"
}

sealed trait Attribute extends Node

case class StringAttribute(var attrType: String, var value: String = "") extends Attribute {
  isAttribute = true
  isStringAttribute = true

  def setValue(newValue: String): StringAttribute = {
    value = newValue
    this
  }

  def setType(newType: String): StringAttribute = {
    attrType = newType
    this
  }

  def getValue: String = value

  def getType: String = attrType

  def modelString = "StringAttribute(\"" + attrType + "\",\"" + value + "\")"

  override def toString(): String = attrType + "(\"" + value + "\")"
}

case class IntAttribute(var attrType: String, var value: Int = 0) extends Attribute {
  isAttribute = true
  isIntAttribute = true

  def setValue(newValue: Int): IntAttribute = {
    value = newValue
    this
  }

  def setType(newType: String): IntAttribute = {
    attrType = newType
    this
  }

  def getValue: Int = value

  def getType: String = attrType

  def modelString = "IntAttribute(\"" + attrType + "\"," + value + ")"

  override def toString(): String = s"$attrType($value)"
}

case class StatusValueAttribute(var attrType: String = "Status", var value: String) extends Attribute{
  isAttribute = true
  isStatusValueAttribute = true

  override def toString(): String = s"$attrType($value)"
}

//class StatusValue extends Enumeration {
//  val ELICITED,SPECIFIED,VALIDATED,PLANNED,IMPLEMENTED,TESTED,RELEASED,FAILED,POSTPONED,DROPPED = Value
//}


case class RelationType(relationType: String) {
  def getType: String = relationType

  def modelString = "RelationType(\"" + relationType + "\")"

  override def toString: String = relationType
}

/**
  * Actions
  */

case class AddElem(path: Seq[String], elem: Elem, relationType: RelationType) extends Action

case class AddElemToPlaceholder(path: Seq[String], elem: Elem) extends Action

case class RemoveElem(path: Seq[String]) extends Action

case class RemoveEmptyRelation(path: Seq[String]) extends Action

case class MoveElemToPlaceholder(oldPath: Seq[String], newPath: Seq[String], afterChildren: Boolean) extends Action

case class MoveElem(oldPath: Seq[String], newPath: Seq[String], relationType: RelationType) extends Action

case class CopyElemToPlaceholder(oldPath: Seq[String], newPath: Seq[String], afterChildren: Boolean) extends Action

case class CopyElem(oldPath: Seq[String], newPath: Seq[String], relationType: RelationType) extends Action

case class UpdateEntity(path: Seq[String], newEntity: Entity) extends Action

case class UpdateIntAttribute(path: Seq[String], newStringAttribute: IntAttribute) extends Action

case class UpdateStringAttribute(path: Seq[String], newIntAttribute: StringAttribute) extends Action

case class UpdateRelation(path: Seq[String], newId: String, newRelationType: Option[RelationType]) extends Action

case class UpdateEntireRelation(path: Seq[String], newEntity: Entity, newRelationType: Option[RelationType]) extends Action

case class SetModel(treeItem: Seq[Elem]) extends Action

case class SetTemplate(nbr: Int) extends Action






