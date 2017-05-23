package example

import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent, ReactKeyboardEventI}
import org.scalajs.dom.ext.KeyCode
import japgolly.scalajs.react._
import shared._


object OrdinalModal {

  def modalStyle = Seq(
    ^.padding := "5px",
    ^.position := "absolute",
    ^.border := "1px solid #CCC",
    ^.borderRadius := "5px",
    ^.top := "40%",
    ^.left := "50%",
    ^.transform := "translate(-50%,-50%)",
    ^.zIndex := "9999",
    ^.background := "#FFF" ,
    ^.paddingBottom := "15px",
    ^.paddingRight := "15px" ,
    ^.paddingTop := "15px",
    ^.paddingLeft := "15px",
    ^.boxShadow := "rgba(0, 0, 0, 0.2) 5px 6px 12px 0px"
  )

  def backdropStyle = Seq(
    ^.position := "absolute",
    ^.width := "100%",
    ^.height := "100%",
    ^.top := "0px",
    ^.left := "0px",
    ^.zIndex := "9998",
    ^.background := "#CCC",
    ^.opacity := "0.5"
  )

  def selectStyle() = Seq(
    ^.className := "form-control pull-right",
    ^.width := "155px",
    ^.height :=  "100%",
    ^.color := "BLACK",
    ^.background := "white",
    ^.textAlign.center,
    ^.textAlignLast.center
  )

  case class State(rankings: Seq[Int] = Seq(), pairs: Seq[Seq[Entity]] = Seq(), deviation: Int = 0, typeToRank: String = "Req", readyForRanking: Boolean = false)

  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback, send: (String => Seq[String]) => Option[Seq[Callback]], currentModel: Tree)


  def generatePairs($: BackendScope[Props, State], entities: Seq[Entity]): Seq[Seq[Entity]] = {
      entities.combinations(2).toList
  }

  def getReq(elems: Seq[Elem], typeToRank: String): Seq[Entity] ={
    if (typeToRank=="")
      getAllEntities(elems).filter(_.entityType.equals("Req"))
    else
      getAllEntities(elems).filter(_.entityType.equals(typeToRank))
  }


  def getAllEntities(elems: Seq[Elem]): Seq[Entity] = {
    if (elems.isEmpty){
      Seq()
    }else{
      val noAttr = elems.filter(_.isEntity).asInstanceOf[Seq[Entity]]
      val relations = elems.filter(_.isRelation)
      val children = relations.flatMap(_.asInstanceOf[Relation].submodel.children)

      noAttr ++ relations.map(_.asInstanceOf[Relation].entity) ++ getAllEntities(children)
    }
  }


  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      if (P.isOpen) {
        if (S.readyForRanking){
          <.div(
            ^.onKeyDown ==> handleKeyDown(P, S),
            <.div(
              modalStyle,
              ^.width := "400px",
              <.h4(
                "Ordinal Ranking",
                ^.textAlign.center
              ),
              <.div(
                S.pairs.zipWithIndex.map(pairs => pairSelect(pairProps(S, pair = pairs._1, index = pairs._2)))
              ),
              <.div(
                "Deviation:",
                <.input(
                  ^.`type` := "number",
                  ^.min := 0,
                  ^.max := 99999999,
                  ^.className := "form-control",
                  ^.width := "60%",
                  ^.borderRadius := "5px",
                  ^.onChange ==> changeDeviation
                )
              ),
              <.div(
                ^.width := "95%",
                ^.padding := "20px",
                ^.display.flex,
                ^.justifyContent.spaceBetween,
                <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick ==> onClose(P)),
                <.button("ta", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick --> tell(S)),
                <.button("OK", ^.className := "btn btn-success pull-right", ^.autoFocus := "true", ^.bottom := "0px", ^.onClick ==> send(P, S))
              )),
            <.div(
              backdropStyle,
              ^.onClick ==> onClose(P)
            )
          )
        }else{
          <.div(
            ^.onKeyDown ==> handleKeyDown(P, S),
            <.div(
              modalStyle,
              ^.width := "400px",
              <.h4(
                "Ordinal Ranking",
                ^.textAlign.center
              ),
              <.div(
                "Choose type to rank",
                entitySelect(P)

              ),
              <.div(
                ^.width := "95%",
                ^.padding := "20px",
                ^.display.flex,
                ^.justifyContent.spaceBetween,
                <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick ==> onClose(P)),
                <.button("Rank", ^.className := "btn btn-success pull-right", ^.autoFocus := "true", ^.bottom := "0px", ^.onClick --> setState(P,S))
              )),
            <.div(
              backdropStyle,
              ^.onClick ==> onClose(P)
            )
          )
        }
      } else
        <.div()

    def setState(P: Props, S:State):  Callback ={
      println("type: " + S.typeToRank)
      val newPairs = generatePairs($, getReq(P.currentModel.children, S.typeToRank))

      $.modState(_.copy(rankings = List.fill(newPairs.size)(1), pairs = newPairs, readyForRanking = true))
    }

    case class pairProps(state: State, pair: Seq[Entity], index: Int)

    val pairSelect = ReactComponentB[pairProps]("pairSelect")
      .render($ =>
        <.div(
          ^.id := $.props.pair.hashCode(),
          ^.className := "btn-group btn-group-justified",
          <.div(
            ^.className :="btn-group",
            <.button(
              ^.backgroundColor := {if ($.props.state.rankings($.props.index) == 0) "green" else "white" },
              ^.`type`:="button",
              ^.className:="btn btn-default",
              ^.onClick --> setRanking(0, $.props.index), // tell($.props.index)
              $.props.pair.head.toString()
            )
          ),
          <.div(
            ^.className :="btn-group",
            <.button(
              ^.backgroundColor := {if ($.props.state.rankings($.props.index) == 1) "green" else "white" },
              ^.`type`:="button",
              ^.className:="btn btn-default",
              ^.onClick --> setRanking(1, $.props.index),
              "Equal"
            )
          ),
          <.div(
            ^.className :="btn-group",
            <.button(
              ^.backgroundColor := {if ($.props.state.rankings($.props.index) == 2) "green" else "white" },
              ^.`type`:="button",
              ^.className:="btn btn-default",
              ^.onClick --> setRanking(2, $.props.index),
              $.props.pair.last.toString()
            )
          )
        )
      )
      .build


    val entitySelect = ReactComponentB[Props]("entitySelect")
      .render($ =>
        <.select(
          selectStyle(),
          ^.onChange ==> setTypeToRank
        )(
          createOptions($.props)
        )
      )
      .build

    def createOptions(P: Props): Seq[TagMod] = {
      val types = findTypesInModel(P.currentModel.children)
      types.map(<.option(_))
    }

    def findTypesInModel(elems: Seq[Elem]): Seq[String] = getAllEntities(elems).map(e => e.entityType).distinct


    def tell(S: State): Callback = Callback(println(S.rankings.size))

    def setTypeToRank(event: ReactEventI): Callback ={
      val input = event.target.value
      $.modState(_.copy(typeToRank = input))
    }

    def changeDeviation(event: ReactEventI): Callback ={
      val input = event.target.value
      $.modState(_.copy(deviation = input.toInt))
    }


    def setRanking(newRank: Int, index: Int): Callback = $.modState(S => S.copy(rankings = S.rankings.updated(index,newRank)))


    def generateRankingList(ranks: Seq[Int], pairs: Seq[Seq[Entity]]): String = {
      val pairsWithRank = pairs.zip(ranks)

      val rank = pairsWithRank.map(p => p._2 match{
        case 0 =>  p._1.head.id + " < " + p._1.last.id
        case 1 =>  p._1.head.id +  " <> "  + p._1.last.id
        case 2 =>  p._1.head.id +  " > "  + p._1.last.id
      } )

      "\""+rank.mkString(";")+ "\""
    }

    def prepOrdinal(state: State, model: String, trams : String): Seq[String] = {
      Seq(s"val ordinalMethod =$model",
      s"val ranked = reqT.parse.comparisonParser.parseAndSolve(ordinalMethod,allowedDeviation=${state.deviation})")
    }

    def resetState: Callback = $.setState(State())

    def send(P: Props, S: State)(e: ReactEvent): Callback ={
      val list = generateRankingList(S.rankings, S.pairs)

      P.send(prepOrdinal(state = S, list, _)) match {
        case Some(callback) => callback.foreach(_.runNow())
        case None => Callback()
      }
      onClose(P)(e)
    }

    def onClose(P: Props)(e: ReactEvent): Callback = P.onClose(e) >> resetState

    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventI): Callback = {
      if (e.nativeEvent.keyCode == KeyCode.Escape) {
        onClose(P)(e)
      }
      else
        Callback()
    }
  }


  val component = ReactComponentB[Props]("Modal")
    .initialState(State(typeToRank = ""))
    .renderBackend[Backend]
    .build


  def apply(isOpen: Boolean, onClose: ReactEvent => Callback, send: (String => Seq[String]) => Option[Seq[Callback]], currentModel: Tree)
  = component.set()(Props(isOpen, onClose, send, currentModel))

}
