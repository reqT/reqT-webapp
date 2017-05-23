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

  object Ranks extends Enumeration {
    type Ranks = Value
    val Less, Equal, Great = Value
  }

  case class State(rankings: Seq[Int] = Seq(), pairs: Seq[Seq[Entity]], deviation: Int = 0)

  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback, send: (String => Seq[String]) => Option[Seq[Callback]], currentModel: Tree)


  def generatePairs(entities: Seq[Entity]): Seq[Seq[Entity]] = {
    entities.combinations(2).toList
  }

  def getReq(elems: Seq[Elem]): Seq[Entity] = {
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

    getAllEntities(elems).filter(_.entityType.equals("Req"))
  }

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P, S),
          <.div(
            modalStyle,
            ^.width:= "400px",
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
                ^.`type`:="number",
                ^.min := 0,
                ^.max := 99999999,
                ^.className := "form-control",
                ^.width := "60%",
                ^.borderRadius := "5px",
                ^.onChange ==> changeDeviation
              )
            ),
            <.div(
              ^.width:= "95%",
              ^.padding := "20px",
              ^.display.flex,
              ^.justifyContent.spaceBetween,
              <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick ==> onClose(P)),
              <.button("OK", ^.className := "btn btn-success pull-right",  ^.autoFocus := "true", ^.bottom := "0px", ^.onClick ==> send(P,S))
            )),
          <.div(
            backdropStyle,
            ^.onClick ==> onClose(P)
          )
        )
      } else
        <.div()

    case class pairProps(state: State, pair: Seq[Entity], index: Int)

    val pairSelect = ReactComponentB[pairProps]("pairSelect")
      .render($ =>
        <.div(
          ^.id := $.props.pair.hashCode(),
          ^.className := "btn-group btn-group-justified",
          <.div(
            ^.className :="btn-group",
            <.button(
//              ^.backgroundColor := {if ($.props.state.rankings($.props.index)._2 == 0) "green" else "white" },
              ^.`type`:="button",
              ^.className:="btn btn-default",
              ^.onClick --> setRanking($.props.pair, 0, $.props.index), // tell($.props.index)
              $.props.pair.head.toString()
            )
          ),
          <.div(
            ^.className :="btn-group",
            <.button(
//              ^.backgroundColor := {if ($.props.state.rankings($.props.index)._2 == 1) "green" else "white" },
              ^.`type`:="button",
              ^.className:="btn btn-default",
              ^.onClick --> setRanking($.props.pair, 1, $.props.index),
              "Equal"
            )
          ),
          <.div(
            ^.className :="btn-group",
            <.button(
//              ^.backgroundColor := {if ($.props.state.rankings($.props.index)._2 == 2) "green" else "white" },
              ^.`type`:="button",
              ^.className:="btn btn-default",
              ^.onClick --> setRanking($.props.pair, 2, $.props.index),
              $.props.pair.last.toString()
            )
          )
        )
      )
      .build

    def tell(index: Int): Callback = Callback(println(index))

    def changeDeviation(event: ReactEventI): Callback ={
      val input = event.target.value
      $.modState(_.copy(deviation = input.toInt))
    }


    def setRanking(pair: Seq[Entity], newRank: Int, index: Int): Callback = $.modState(S => S.copy(rankings = S.rankings.updated(index,newRank)))

//    def generateRankingList(ranks: Seq[Int], pairs: Seq[Entity]): String = {
//      val rank = pairsWithRank.map(p => p._2 match{
//        case 0 => (p._1.head.id,"<", p._1.last.id)
//        case 1 => (p._1.head.id,"<>", p._1.last.id)
//        case 2 => (p._1.head.id,">", p._1.last.id)
//      } )
//      "\""+rank.map(s => s._1 ++ s._2 ++ s._3).mkString(" ")+"\""
//    }

    def prepOrdinal(state: State, model: String, trams : String): Seq[String] = {
      Seq(s"val ordinalMethod =$model",
      s"val ranked = reqT.parse.comparisonParser.parseAndSolve(ordinalMethod,allowedDeviation=${state.deviation})")
    }

//    def resetState: Callback = $.setState(State(Seq()))

    def send(P: Props, S: State)(e: ReactEvent): Callback ={
//      val list = generateRankingList(S.rankings, S.pairs)

      P.send(prepOrdinal(state = S, "hej", _)) match {
        case Some(callback) => callback.foreach(_.runNow())
        case None => Callback()
      }
      onClose(P)(e)
    }

    def onClose(P: Props)(e: ReactEvent): Callback = P.onClose(e) //>> resetState

    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventI): Callback = {
      if (e.nativeEvent.keyCode == KeyCode.Escape) {
        onClose(P)(e)
      }
      else
        Callback()
    }
  }


  val component = ReactComponentB[Props]("Modal")
    .initialState_P(P => State(pairs = generatePairs(getReq(P.currentModel.children)) ))
    .renderBackend[Backend]
    .build


  def apply(isOpen: Boolean, onClose: ReactEvent => Callback, send: (String => Seq[String]) => Option[Seq[Callback]], currentModel: Tree)
  = component.set()(Props(isOpen, onClose, send, currentModel))

}
