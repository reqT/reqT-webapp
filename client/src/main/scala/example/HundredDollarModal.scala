package example

import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent, ReactKeyboardEventI}
import org.scalajs.dom.ext.KeyCode
import japgolly.scalajs.react._
import shared._


object HundredDollarModal {

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

  case class State(newSHSEntity: Option[Entity], newRSEntity: Option[Entity], newBAttribute: Option[Attribute], newPAttribute: Option[Attribute])

  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback, send: (String => String) => Option[Callback])

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P, S),
          <.div(
            modalStyle,
            ^.width:= "400px",
            <.h4(
              "100 Dollar Method",
              ^.textAlign.center
            ),
            <.dl(
              ^.className := "dl-horizontal",
              <.dt(
                ^.textAlign.center,
                "Normalize: "
              ),
              <.dd(
                AttributeSelect("Benefit", isIntAttr = true, setNewBAttribute)
              ),
              <.dt(
                ^.textAlign.center,
                " from "
              ),
              <.dd(
                EntitySelect("Req", setNewRSEntity, isModelValue = false)
              ),
              <.hr,
              <.dt(
                ^.textAlign.center,
                "Weight on: "
              ),
              <.dd(
                AttributeSelect("Prio", isIntAttr = true, setNewPAttribute)
              ),
              <.dt(
                ^.textAlign.center,
                " from "
              ),
              <.dd(
                EntitySelect("Stakeholder", setNewSHSEntity, isModelValue = false)
              )
//              <.hr
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


    def prepHundredDollar(state: State, model: String): String = {
      val shs = state.newSHSEntity.getOrElse("Stakeholder").toString.split('(').head
      val rs = state.newRSEntity.getOrElse("Req").toString.split('(').head
      val b = state.newBAttribute.getOrElse("Benefit").toString.split('(').head
      val p = state.newPAttribute.getOrElse("Prio").toString.split('(').head

      val dollarMethod =s"\n val shs = m . entitiesOfType ( $shs ) \n val rs = m . entitiesOfType ( $rs ) \n val prioSum = shs . map ( s => m / s / $p ) . sum " +
        s"\n val benefitSum = shs . map ( s => s -> ( m / s ) . collect { case $b ( b ) => b }. sum ) . toMap " +
        s"\n val hundreddollarmethod = rs . map ( r => r has $b  ( math . round ( shs . map ( s =>( m / s / $p ) *( m / s / r / $b ) *100.0 / ( benefitSum ( s ) * prioSum ) ) . sum ) . toInt ) ) . toModel " +
        s"\n val sum = hundreddollarmethod . collect { case $b  ( b ) => b }. sum"

      "val m="+model+dollarMethod
    }

    def resetState: Callback = $.setState(State(None,None,None,None))

    def setNewSHSEntity(entity: Option[Entity]): Callback = $.modState(_.copy(newSHSEntity = entity))
    def setNewRSEntity(entity: Option[Entity]): Callback = $.modState(_.copy(newRSEntity = entity))

    def setNewBAttribute(attribute: Option[Attribute]): Callback = $.modState(_.copy(newBAttribute = attribute))
    def setNewPAttribute(attribute: Option[Attribute]): Callback = $.modState(_.copy(newPAttribute = attribute))

    def send(P: Props, S: State)(e: ReactEvent): Callback =
      P.send(prepHundredDollar(state = S, _)) match {
        case Some(callback) => callback >> onClose(P)(e)
        case None => Callback()
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
      .initialState(State(None,None,None,None))
      .renderBackend[Backend]
      .build


    def apply(isOpen: Boolean, onClose: ReactEvent => Callback, send: (String => String) => Option[Callback])
    = component.set()(Props(isOpen, onClose, send))

}
