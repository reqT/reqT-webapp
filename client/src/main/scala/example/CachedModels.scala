package example

import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB}
import shared.{SetModel, Tree, UUID}
import japgolly.scalajs.react._

import scala.collection.immutable.Queue

/**
  * Created by phiped on 5/25/17.
  */
object CachedModels {

  val cachedModelStyle = Seq(
    ^.padding := "5px",
    ^.paddingRight := "5px",
    ^.height := "5%",
    ^.overflow := "hidden",
    ^.position.relative
  )

  val addButtonStyle = Seq(
    ^.className := "glyphicon glyphicon-plus",
    ^.color := "green",
    ^.position.absolute,
    ^.top := "0%",
    ^.width := "5%",
    ^.height := "105%",
    ^.marginLeft := "-6px",
    ^.marginTop := "-2px",
    ^.outline := "none"
  )
  // Ska nog in i addButtonStyle
//  addClassName("btn btn-default navbar-btn"),
//  margin(5.px),
//  padding(10.px)

  val tabAreaStyle = Seq(
    ^.overflowX.auto,
    ^.left := "5%",
    ^.height := "91%",
    ^.overflowX.auto,
    ^.overflowY.hidden,
    ^.width := "95%",
    ^.position.absolute
  )

  val tabDivStyle = Seq(
    ^.whiteSpace := "nowrap",
    ^.position.absolute,
    ^.className := "clickable-row"
  )

  val listStyle = Seq(
    ^.display.flex,
    ^.height := "0px",
    ^.className := "nav nav-pills",
    ^.listStyleType.none
  )

  val tabStyle = Seq(
    ^.className := "navpill",
    ^.display := "inline",
    ^.whiteSpace := "nowrap",
    ^.position.relative,
    ^.marginLeft := "5px",
    ^.marginRight := "5px",
    ^.padding := "5px",
    ^.float.left,
    ^.overflow := "hidden",
    ^.borderRadius := "5px",
    ^.height := "30px",
    ^.top := "0px",
    ^.width := "200px",
    ^.background := "#CFEADD"
  )

  val tabSpanStyle = Seq(
    ^.className := "col",
    ^.position.absolute,
    ^.width := "80%",
    ^.height := "30px",
    ^.paddingTop := "2px",
    ^.textAlign := "center"
  )

  val deleteTabButtonStyle = Seq(
//    ^.className := "col",
    ^.position.absolute,
    ^.width := "20%",
    ^.height := "30px",
    ^.left := "80%",
    ^.top := "0%",
    ^.paddingTop := "5px",
    ^.outline := "none",
    ^.className := "btn glyphicon glyphicon-remove pull-right"
  )

  case class CachedModel(name: String, model: Tree, selected: Boolean, uUID: UUID)

  case class State(isModalOpen: Boolean = false, modalType: String = "save",
                    cachedModels: Queue[CachedModel] = Queue(CachedModel("untitled", Tree(Seq()), selected = true, uUID = UUID.random())))

  case class Props(modelProxy: ModelProxy[Tree])

  class Backend($: BackendScope[Props, State]) {

    def openModal(newModelType: String) = $.modState(_.copy(isModalOpen = true, modalType = newModelType))

    def closeModal(): Callback = $.modState(S => S.copy(isModalOpen = false))

    def setActiveModel(model: CachedModel,P: Props, S: State): Callback = {
      updateActiveModel(model, P,S)
    }

    def saveModel(name: String, model: Tree, P: Props): Callback ={
      println("asdfasfd")
      $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, model, selected = false, UUID.random())))
    }

    def updateActiveModel(model: CachedModel, P: Props, S: State): Callback = {
      val newModels: Queue[CachedModel] = S.cachedModels.map(model =>
        if(model.selected && model.uUID.equals(model.uUID))
            model.copy(model = P.modelProxy.value, selected = true)
        else if (model.selected)
            model.copy(model = P.modelProxy.value, selected = false)
        else if(model.uUID.equals(model.uUID))
          model.copy(selected = true)
        else model

        // Borde funka
//        if (model.selected)
//          model.copy(model = P.modelProxy.value, selected = model.uUID.equals(model.uUID))
//        else
//          model.copy(selected = model.uUID.equals(model.uUID))

      )

      $.modState(_.copy(cachedModels = newModels))
    }

    def activeModel(activeModel: CachedModel, cachedModels: Queue[CachedModel]): Callback = $.modState(_.copy(cachedModels = cachedModels.map(
      model => if(model.uUID.equals(activeModel.uUID)) model.copy(selected = true) else model.copy(selected = false))))

    def removeCachedModel(state: State, modelToRemove: CachedModel)(e: ReactEventI): Callback = {
      e.stopPropagation()
      val index = state.cachedModels.indexWhere(_.equals(modelToRemove))
      val beginning = state.cachedModels.take(index)
      val end = state.cachedModels.drop(index+1)
      $.modState(_.copy(cachedModels = beginning ++ end))
    }


    val cachedModels = ReactComponentB[(Props, State)]("cachedModelsComponent")
      .render( $ => <.pre(
        cachedModelStyle,
        <.button(
          addButtonStyle,
          ^.onClick --> openModal("save")
        ),
        <.div(
          tabAreaStyle,
          <.div(
            tabDivStyle,
            <.ul(
              listStyle,
              $.props._2.cachedModels.reverse.map(model => listModels((model, $.props._1, $.props._2)))
            )
          )
        )
      )
      ).build


    val listModels = ReactComponentB[(CachedModel, Props, State)]("listElem")
      .render($ => <.li(
        tabStyle,
        ^.opacity := {if($.props._1.selected) "1" else "0.5"},
        <.span(
          tabSpanStyle,
          $.props._1.name
        ),
        ^.onClick --> (setActiveModel($.props._1, $.props._2, $.props._3) >> $.props._2.modelProxy.dispatchCB(SetModel($.props._1.model.children))),

        <.button(
          deleteTabButtonStyle,
          ^.onClick ==> removeCachedModel($.props._3, $.props._1)
        )
      )).build



    def render(P: Props, S: State) = {
     <.div(
       cachedModelStyle,
      NewModelModal(isOpen =  S.isModalOpen, onClose = closeModal, saveModel = saveModel(_, _, P), tree = P.modelProxy.value, modalType =  S.modalType),
      cachedModels((P,S))
      )
    }

  }

  val component = ReactComponentB[Props]("Modal")
  .initialState(State())
  .renderBackend[Backend]
  .build


  def apply(modelProxy: ModelProxy[Tree])
  = component.set()(Props(modelProxy))

}
