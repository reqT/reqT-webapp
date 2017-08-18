package main

import diode._
import diode.react.ReactConnector
import shared._

object AppCircuit extends Circuit[Model] with ReactConnector[Model] {

  def initialModel: Model = Model(Tree(Seq()))

  def zoomToChildren(modelRW: ModelRW[Model, Tree], path: Seq[String]): Option[ModelRW[Model, Seq[Elem]]] = {
    if (path.isEmpty) {
      Some(modelRW.zoomTo(_.children))
    } else {
      val currentElem = path.head
      val index = modelRW.value.children.indexWhere(retrieveEntity(_).uuid.toString == currentElem)
      val elemFound = retrieveEntity(modelRW.value.children(index))
      if (elemFound.hasRelation) {
        val elemRW = modelRW.zoomRW(_.children.apply(index))((tree, newElem)
        => tree.copy(children = (tree.children.take(index) :+ newElem) ++ tree.children.drop(index + 1)))

        zoomToChildren(elemRW.zoomRW(_.asInstanceOf[Relation].submodel)((relation, newSubmodel)
        => relation.asInstanceOf[Relation].copy(submodel = newSubmodel)), path.tail)

      } else if (elemFound.isEntity) {
        zoomToChildren(modelRW, path.tail)
      } else {
        zoomToChildren(modelRW, path.tail)
      }
    }
  }

  def zoomToRelation(modelRW: ModelRW[Model, Tree], path: Seq[String]): Option[ModelRW[Model, Elem]] = {
    val index = modelRW.value.children.indexWhere(retrieveEntity(_).uuid.toString == path.head)
    val elemFound = retrieveEntity(modelRW.value.children(index))

    if (elemFound.hasRelation) {
      val elemRW = modelRW.zoomRW(_.children.apply(index))((tree, newElem)
      => tree.copy(children = (tree.children.take(index) :+ newElem) ++ tree.children.drop(index + 1)))

      if (!elemRW.value.isRelation || path.size == 1) {
        Some(elemRW)
      } else {
        zoomToRelation(elemRW.zoomRW(_.asInstanceOf[Relation].submodel)((relation, newSubmodel)
        => relation.asInstanceOf[Relation].copy(submodel = newSubmodel)), path.tail)
      }
    } else {
      None
    }
  }

  def copyElem(elem: Elem): Elem = elem match {
    case relation: Relation => Relation(copyElem(relation.entity).asInstanceOf[Entity], relation.link, copyTree(relation.submodel))
    case entity: Entity => Entity(entity.entityType, entity.id)
    case intAttr: IntAttribute => IntAttribute(intAttr.attrType, intAttr.value)
    case stringAttr: StringAttribute => StringAttribute(stringAttr.attrType, stringAttr.value)
  }

  def copyTree(tree: Tree): Tree = Tree(tree.children.map(copyElem))

  def retrieveEntity(elem: Elem): Elem = elem match {
    case relation: Relation => relation.entity
    case node: Node => node
  }

  def findElem(modelRW: ModelRW[Model, Tree], path: Seq[String], elemUUID: String): Elem = {
    zoomToChildren(modelRW, path) match {
      case Some(modelRW) => modelRW.value.find(retrieveEntity(_).uuid.toString == elemUUID).get
      case None => Entity("Error", "404")
    }
  }

  def addElem(elemToAdd: Elem, list: Seq[Elem], elemUUID: String): Seq[Elem] = {

    val end = list.dropWhile(retrieveEntity(_).uuid.toString != elemUUID).seq
    val beginning = list.takeWhile(retrieveEntity(_).uuid.toString != elemUUID).seq

    if (beginning.nonEmpty && end.nonEmpty)
      (beginning :+ end.head) ++ (elemToAdd +: end.tail)
    else if (beginning.isEmpty && end.nonEmpty)
      end.head +: (elemToAdd +: end.tail)
    else if (beginning.nonEmpty && end.isEmpty)
      beginning :+ elemToAdd
    else
      Seq(elemToAdd)
  }


  class TreeHandler[M](modelRW: ModelRW[Model, Tree]) extends ActionHandler(modelRW) {
    override def handle = {

      case AddElem(path: Seq[String], newElem: Elem, relationType: RelationType) =>
        val elemUUID = path.last

        zoomToChildren(modelRW, path.tail) match {
          case Some(modelRW) =>
            modelRW.value.find(_.uuid.toString == path.last) match {
              case Some(foundElem) => updated(modelRW.updated(modelRW.value.map(
                elem => if (elem.uuid.toString == foundElem.uuid.toString && !elem.isAttribute) Relation(elem.asInstanceOf[Entity], relationType, Tree(Seq(newElem))) else elem
              )).tree)

              case None => updated(modelRW.updated(modelRW.value :+ newElem).tree)
            }
          case None =>
            noChange
        }


      case AddElemToPlaceholder(path: Seq[String], newElem: Elem) =>
        zoomToChildren(modelRW, path.tail) match {
          case Some(rw) =>
            rw.value.find(_.uuid.toString == path.last) match {
              case Some(_) => updated(rw.updated(addElem(newElem, rw.value, path.last)).tree)
              case None => updated(rw.updated(newElem +: rw.value).tree)
            }
          case None => noChange
        }


      case RemoveEmptyRelation(path: Seq[String]) => {
        if (path.size == 2) {
          updated(Tree(modelRW.value.children collect {
            case relation: Relation =>
              if (retrieveEntity(relation).uuid.toString == path.last && relation.submodel.children.isEmpty)
                retrieveEntity(relation).getWithRelation(false)
              else relation
            case elem: Elem => elem
          }))

        } else if (path.size > 2) {
          zoomToChildren(modelRW, path.tail.init) match {
            case Some(rw) =>
              updated(rw.updated(rw.value collect {
                case relation: Relation =>
                  if (retrieveEntity(relation).uuid.toString == path.last && relation.submodel.children.isEmpty)
                    retrieveEntity(relation).getWithRelation(false)
                  else relation
                case elem: Elem => elem
              }).tree)

            case None => noChange
          }
        } else {
          noChange
        }
      }

      case RemoveElem(path: Seq[String]) =>
        if (path.isEmpty)
          noChange
        else if (path.size == 1)
          updated(Tree(Seq()))
        else {
          val elemUUID = path.last

          zoomToChildren(modelRW, path.init.tail) match {
            case Some(modelRW) => updated(modelRW.updated(modelRW.value.filterNot(retrieveEntity(_).uuid.toString == elemUUID)).tree)

            case None => noChange
          }
        }

      case MoveElem(oldPath: Seq[String], newPath: Seq[String], relationType: RelationType) =>
        val elemUUID = oldPath.last
        val elemToMove: Elem = findElem(modelRW, oldPath.init.tail, elemUUID)

        zoomToChildren(modelRW, newPath.tail) match {
          case Some(rw) =>
            rw.value.find(_.uuid.toString == newPath.last) match {
              case Some(foundElem) =>
                updated(rw.updated(rw.value.map(
                  elem => if (elem.uuid.toString == foundElem.uuid.toString) Relation(elem.asInstanceOf[Entity],
                    relationType,
                    Tree(Seq(elemToMove))) else elem
                )).tree)
              case None =>
                updated(rw.updated(rw.value :+ elemToMove).tree)
            }
          case None => noChange
        }


      case MoveElemToPlaceholder(oldPath: Seq[String], newPath: Seq[String], afterChildren: Boolean) =>
        val elemUUID = oldPath.last
        val elemToMove: Elem = copyElem(findElem(modelRW, oldPath.init.tail, elemUUID))

        if (afterChildren) {
          if (newPath.size == 1) {
            updated(Tree(modelRW.value.children :+ elemToMove))
          }
          else {
            zoomToChildren(modelRW, newPath.tail.init) match {
              case Some(rw) => updated(rw.updated(addElem(elemToMove, rw.value, newPath.last)).tree)
              case None => noChange
            }
          }
        } else {
          zoomToChildren(modelRW, newPath.tail) match {
            case Some(rw) =>
              rw.value.find(_.uuid.toString == newPath.last) match {
                case Some(_) => updated(rw.updated(addElem(elemToMove, rw.value, newPath.last)).tree)
                case None => updated(rw.updated(elemToMove +: rw.value).tree)
              }
            case None => noChange
          }
        }

      case CopyElemToPlaceholder(oldPath: Seq[String], newPath: Seq[String], afterChildren: Boolean) =>
        val elemUUID = oldPath.last
        val elemToCopy: Elem = findElem(modelRW, oldPath.init.tail, elemUUID)
        val copiedElem = copyElem(elemToCopy)

        if (afterChildren) {
          if (newPath.size == 1) {
            updated(Tree(modelRW.value.children :+ copiedElem))
          }
          else {
            zoomToChildren(modelRW, newPath.tail.init) match {
              case Some(rw) => updated(rw.updated(addElem(copiedElem, rw.value, newPath.last)).tree)
              case None => noChange
            }
          }
        } else {
          zoomToChildren(modelRW, newPath.tail) match {
            case Some(rw) =>
              rw.value.find(_.uuid.toString == newPath.last) match {
                case Some(_) => updated(rw.updated(addElem(copiedElem, rw.value, newPath.last)).tree)
                case None => updated(rw.updated(copiedElem +: rw.value).tree)
              }
            case None => noChange
          }
        }

      case SetModel(treeItem: Seq[Elem]) =>
        val model = Tree(treeItem)
        updated(model)


      case CopyElem(oldPath: Seq[String], newPath: Seq[String], relationType: RelationType) =>
        val elemUUID = oldPath.last
        val elemToCopy: Elem = findElem(modelRW, oldPath.init.tail, elemUUID)
        val copiedElem = copyElem(elemToCopy)

        zoomToChildren(modelRW, newPath.tail) match {
          case Some(rw) =>
            rw.value.find(_.uuid.toString == newPath.last) match {
              case Some(foundElem) =>
                updated(rw.updated(rw.value.map(
                  elem => if (elem.uuid.toString == foundElem.uuid.toString) Relation(elem.asInstanceOf[Entity],
                    relationType,
                    Tree(Seq(copiedElem))) else elem
                )).tree)
              case None => updated(rw.updated(rw.value :+ copiedElem).tree)
            }
          case None => noChange
        }

      case UpdateEntity(path: Seq[String], newEntity: Entity) =>
        val elemID = path.last

        zoomToChildren(modelRW, path.tail) match {
          case Some(modelRW) =>
            updated(modelRW.updated(modelRW.value.map(elem => if (retrieveEntity(elem).uuid.toString == elemID) {
              newEntity.setUUID(retrieveEntity(elem).uuid)
              newEntity
            }
            else elem)).tree)
          case None => noChange
        }


      case UpdateIntAttribute(path: Seq[String], newIntAttribute: IntAttribute) =>
        val elemID = path.last

        zoomToChildren(modelRW, path.tail) match {
          case Some(modelRW) =>
            updated(modelRW.updated(modelRW.value.map(elem => if (retrieveEntity(elem).uuid.toString == elemID) {
              newIntAttribute.setUUID(retrieveEntity(elem).uuid)
              newIntAttribute
            }
            else elem)).tree)

          case None => noChange
        }


      case UpdateStringAttribute(path: Seq[String], newStringAttribute: StringAttribute) =>
        val elemID = path.last

        zoomToChildren(modelRW, path.tail) match {
          case Some(modelRW) =>
            updated(modelRW.updated(modelRW.value.map(elem => if (retrieveEntity(elem).uuid.toString == elemID) {
              newStringAttribute.setUUID(retrieveEntity(elem).uuid)
              newStringAttribute
            }
            else elem)).tree)

          case None => noChange
        }


      case UpdateRelation(path: Seq[String], newId: String, newRelationType: Option[RelationType]) =>
        zoomToRelation(modelRW, path.tail) match {
          case Some(modelRW) => updated(modelRW.updated(modelRW.value.asInstanceOf[Relation].setLink(newRelationType.getOrElse(modelRW.value.asInstanceOf[Relation].link)).setEntityID(newId)).tree)

          case None => noChange
        }

      case UpdateEntireRelation(path: Seq[String], newEntity: Entity, newRelationType: Option[RelationType]) =>
        zoomToRelation(modelRW, path.tail) match {
          case Some(modelRW) => updated(modelRW.updated(modelRW.value.asInstanceOf[Relation].setLink(newRelationType.getOrElse(modelRW.value.asInstanceOf[Relation].link)).setEntity(newEntity)).tree)

          case None => noChange
        }
      case NoAction => noChange
    }
  }

  override protected def actionHandler = new TreeHandler(zoomTo(_.tree))
}
