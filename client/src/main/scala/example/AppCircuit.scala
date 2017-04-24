package example

import diode._
import diode.react.ReactConnector

object AppCircuit extends Circuit[Model] with ReactConnector[Model] {

  def initialModel: Model = Model(Tree(Seq(Req("R1"), Req("R2"), Stakeholder("BOSS"),
    Relation(Req("R3"), has, Tree(Seq(Relation(Req("R3.1"), has, Tree(Seq(Prio(1))))))), Relation(Req("R4"), has, Tree(Seq(Prio(2)))))))

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

        if(!elemRW.value.isRelation || path.size == 1){
          Some(elemRW)
        } else {
          zoomToRelation(elemRW.zoomRW(_.asInstanceOf[Relation].submodel)((relation, newSubmodel)
          => relation.asInstanceOf[Relation].copy(submodel = newSubmodel)), path.tail)
        }
      } else {
        None
      }
  }

  def retrieveEntity(elem: Elem): Elem = elem match {
    case relation: Relation => relation.entity
    case node: Node => node
  }


  class TreeHandler[M](modelRW: ModelRW[Model, Tree]) extends ActionHandler(modelRW) {
    override def handle = {
      //case Reset => updated(Tree(Seq()))

      case AddElem(path: Seq[String], newElem: Elem, relationType: RelationType) =>
        zoomToChildren(modelRW, path.tail) match {
          case Some(modelRW) =>
            modelRW.value.find(_.uuid.toString == path.last) match {
              case Some(foundElem) => updated(modelRW.updated(modelRW.value.map(
                elem => if (elem == foundElem && !elem.isAttribute) Relation(elem.asInstanceOf[Entity], relationType, Tree(Seq(newElem))) else elem
              )).tree)

              case None => updated(modelRW.updated(modelRW.value :+ newElem).tree)
            }
          case None =>
            noChange
        }

      case RemoveEmptyRelation(path: Seq[String]) => {
        if(path.size == 2) {
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
        println(path)
        if (path.isEmpty)
          noChange
        else if (path.size == 1)
          updated(Tree(Seq()))
        else {
          val elemToRemove = path.last

          zoomToChildren(modelRW, path.init.tail) match {
            case Some(modelRW) => updated(modelRW.updated(modelRW.value.filterNot(retrieveEntity(_).uuid.toString == elemToRemove)).tree)

            case None => noChange
          }
        }

      case MoveElem(oldPath: Seq[String], newPath: Seq[String], relationType: RelationType) =>
          var element: Elem = Req()
          val elemToRemove = oldPath.last

          zoomToChildren(modelRW, oldPath.init.tail) match {
            case Some(modelRW) =>
              element = modelRW.value.find(retrieveEntity(_).uuid.toString == elemToRemove).get
            case None => noChange
          }

          zoomToChildren(modelRW, newPath.tail) match {
            case Some(rw) =>
              rw.value.find(_.uuid.toString == newPath.last) match {
                case Some(foundElem) =>
                  updated(rw.updated(rw.value.map(
                    elem => if (elem == foundElem) Relation(elem.asInstanceOf[Entity],
                      relationType,
                      Tree(Seq(element))) else elem
                  )).tree)
                case None => updated(rw.updated(rw.value :+ element).tree)
              }
            case None => noChange
          }


      case SetTemplate =>
        val model = Tree(Seq(
          Relation(Goal("accuracy"), has, Tree(Seq(Spec("Our pre-calculations shall hit within 5%")))),
          Relation(Feature("quotation"), has, Tree(Seq(Spec("Product shall support cost recording and quotation with experience data")))),
          Relation(Function("experienceData"), has, Tree(Seq(Spec("Product shall have recording and retrieval functions for experience data")))),
          Relation(Design("screenX"), has, Tree(Seq(Spec("System shall have screen pictures as shown in Fig. X"))))
        ))
        updated(model)

      case SetTemplate1 =>
        val model = Tree(Seq(Req("R1"), Req("R2"), Stakeholder("BOSS"),
          Relation(Req("R3"), has, Tree(Seq(Relation(Req("R3.1"), has, Tree(Seq(Prio(1))))))), Relation(Req("R4"), has, Tree(Seq(Prio(2))))))

        updated(model)

      case UpdateEntity(path:Seq[String], newEntity: Entity) =>
        val elemID = path.last

        zoomToChildren(modelRW, path.tail) match {
          case Some(modelRW) =>
            updated(modelRW.updated(modelRW.value.map(elem => if(retrieveEntity(elem).uuid.toString == elemID){
              newEntity.setUUID(retrieveEntity(elem).uuid)
              newEntity
            }
              else elem)).tree)
        }

      case UpdateIntAttribute(path: Seq[String], newValue: Int) =>
        val elemID = path.last

        zoomToChildren(modelRW, path.tail) match {
          case Some(modelRW) =>
            updated(modelRW.updated(modelRW.value.map(elem => if (retrieveEntity(elem).uuid.toString == elemID)
                elem.asInstanceOf[IntAttribute].setValue(newValue) else elem)).tree)

          case None => noChange
        }


      case UpdateStringAttribute(path: Seq[String], newValue: String) =>
        val elemID = path.last

        zoomToChildren(modelRW, path.tail) match {
          case Some(modelRW) =>
            updated(modelRW.updated(modelRW.value.map(elem => if (retrieveEntity(elem).uuid.toString == elemID)
              elem.asInstanceOf[StringAttribute].setValue(newValue) else elem)).tree)

          case None => noChange
        }


      case UpdateRelation(path: Seq[String], newId: String, newRelationType: Option[RelationType]) =>
        zoomToRelation(modelRW, path.tail) match {
          case Some(modelRW) => updated(modelRW.updated(modelRW.value.asInstanceOf[Relation].setLink(newRelationType.getOrElse(modelRW.value.asInstanceOf[Relation].link)).setEntity(newId)).tree)

          case None => noChange
        }

      case NoAction => noChange
    }
  }

  override protected def actionHandler = new TreeHandler(zoomTo(_.tree))
}
