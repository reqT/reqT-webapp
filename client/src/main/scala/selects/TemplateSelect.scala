package selects

import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, _}
import org.scalajs.dom.ext.KeyCode
import shared._


object TemplateSelect {

  case class State(isOpen: Boolean = false)

  case class Props(openNewModelModal: (String, Tree) => Callback)

  def contentStyle = Seq(
    ^.marginTop := "11px",
    ^.height := "70px",
    ^.width := "100px",
    ^.position := "absolute",
    ^.backgroundColor := "#f9f9f9",
    ^.minWidth := "200px",
    ^.zIndex := "9999"
  )

  def backdropStyle = Seq(
    ^.position := "absolute",
    ^.width := "1000px",
    ^.height := "1000px",
    ^.top := "0px",
    ^.left := "0px",
    ^.zIndex := "9998"
  )

  val templates = Seq("Goal-Design scale", "Why + Spec + Example", "Context Diagram I", "Context Diagram II",
    "Data dictionary", "State transition model", "Model with section", "Prioritization $100 method", "Prioritization Ordinal Ranking",
    "Hotel Reception", "Quper", "Quality Requirements", "Variability Modelling", "Release Planning I", "Release Planning II")

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      <.button(
        ^.onKeyDown ==> handleKeyDown,
        ^.className := "btn btn-default navbar-btn",
        ^.margin := "5px",
        ^.padding := "10px",
        "Templates ",
        ^.onClick --> $.modState(_.copy(isOpen = !S.isOpen)),
        <.span(^.className := "caret"),
        S.isOpen ?= dropdownList(P),
        S.isOpen ?= <.div(
          backdropStyle,
          ^.onClick --> closeDropdown
        )
      )


    val dropdownList = ReactComponentB[Props]("dropdownList")
      .render(P =>
        <.div(
          contentStyle,
          templates.map(s => template((P.props, s)))
        )
      )
      .build


    val template = ReactComponentB[(Props, String)]("template")
      .render(P =>
        <.div(
          ^.className := "list-group-item",
          ^.boxShadow := "0px 8px 16px 0px rgba(0,0,0,0.2)",
          P.props._2,
          ^.onClick ==> onClick(P.props._1)
        )
      ).build

    def handleKeyDown(event: ReactKeyboardEventI): Callback = {
      if (event.nativeEvent.keyCode == KeyCode.Enter) {
        event.preventDefault()
        closeDropdown
      } else if (event.nativeEvent.keyCode == KeyCode.Escape) {
        event.preventDefault()
        closeDropdown
      }
      else
        Callback()
    }

    def closeDropdown: Callback = $.modState(_.copy(isOpen = false))


    def onClick(P: Props)(e: ReactEventI): Callback = {
      e.preventDefault()
      val template = e.target.textContent.toString

      val T = template match {
        case "Goal-Design scale" => m1
        case "Why + Spec + Example" => m2
        case "Context Diagram I" => m3
        case "Context Diagram II" => m4
        case "Data dictionary" => m5
        case "State transition model" => m6
        case "Model with section" => m7
        case "Prioritization $100 method" => m8
        case "Prioritization Ordinal Ranking" => m9
        case "Hotel Reception" => m10
        case "Quper" => m11
        case "Quality Requirements" => m12
        case "Variability Modelling" => m13
        case "Release Planning I" => m14
        case "Release Planning II" => m15
      }
      P.openNewModelModal("temp", Tree(T))

    }
  }

  val component = ReactComponentB[Props]("TemplateSelect")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(openNewModelModal: (String, Tree) => Callback) = component.set()(Props(openNewModelModal))


  val m1 = Vector(Relation(Entity("Goal", "accuracy"), RelationType("has"), Tree(Seq(StringAttribute("Spec", "Our pre-calculations shall hit within 5%")))), Relation(Entity("Feature", "quotation"), RelationType("has"), Tree(Seq(StringAttribute("Spec", "Product shall support cost recording and quotation with experience data")))), Relation(Entity("Function", "experienceData"), RelationType("has"), Tree(Seq(StringAttribute("Spec", "Product shall have recording and retrieval functions for experience data")))), Relation(Entity("Design", "screenX"), RelationType("has"), Tree(Seq(StringAttribute("Spec", "System shall have screen pictures as shown in Fig. X")))))

  val m2 = Vector(Relation(Entity("Feature", "navigate"), RelationType("has"), Tree(Vector(StringAttribute("Why", "Measuring neural response is a bit painful to the  patient. Electrodes must be kept in place ... So both hands should be at the patient during a measurement."), StringAttribute("Spec", "It shall be possible to perform the commands start, stop, ... with both hands at the patient."), StringAttribute("Example", "Might be done with mini keyboard (wrist keys), foot pedal, voice recognition, etc.")))))

  val m3 = Vector(Relation(Entity("Product", "hotelApp"), RelationType("interactsWith"), Tree(Vector(Entity("User", "receptionist"), Entity("User", "guest"), Entity("System", "accounting")))), Relation(Entity("User", "guest"), RelationType("interactsWith"), Tree(Vector(Entity("Product", "hotelApp")))), Relation(Entity("User", "receptionist"), RelationType("interactsWith"), Tree(Vector(Entity("Product", "hotelApp")))), Relation(Entity("System", "telephony"), RelationType("interactsWith"), Tree(Vector(Entity("Product", "hotelApp")))))

  val m4 = Vector(Relation(Entity("Product", "HotelApp"), RelationType("has"), Tree(Vector(Relation(Entity("Interface", "receptionUI"), RelationType("has"), Tree(Vector(Entity("Actor", "Receptionist")))), Relation(Entity("Interface", "guestUI"), RelationType("has"), Tree(Vector(Entity("Actor", "Guest")))), Relation(Entity("Interface", "phoneAPI"), RelationType("has"), Tree(Vector(Entity("System", "Telephony")))), Relation(Entity("Interface", "accountAPI"), RelationType("has"), Tree(Vector(Entity("System", "Accounting"))))))), Relation(Entity("Data", "InterfaceIO"), RelationType("has"), Tree(Vector(Relation(Entity("Interface", "receptionUI"), RelationType("has"), Tree(Vector(Entity("Input", "booking"), Entity("Input", "checkOut"), Entity("Output", "serviceNote")))), Relation(Entity("Interface", "guestUI"), RelationType("has"), Tree(Vector(Entity("Output", "confirmation"), Entity("Output", "invoice"))))))))

  val m5 = Vector(Relation(Entity("Section", "relations"), RelationType("has"), Tree(Vector(Relation(Entity("Class", "Guest"), RelationType("relatesTo"), Tree(Vector(Entity("Class", "Stay"), IntAttribute("Min", 1)))), Relation(Entity("Class", "Stay"), RelationType("relatesTo"), Tree(Vector(Entity("Class", "RoomState"), Entity("Class", "Service"), IntAttribute("Min", 1)))), Relation(Entity("Class", "ServiceType"), RelationType("relatesTo"), Tree(Vector(Entity("Class", "Service"), IntAttribute("Min", 1)))), Relation(Entity("Class", "Room"), RelationType("relatesTo"), Tree(Vector(Entity("Class", "RoomState"), IntAttribute("Min", 1))))))), Relation(Entity("Section", "attributes"), RelationType("has"), Tree(Vector(Relation(Entity("Class", "Guest"), RelationType("has"), Tree(Vector(Entity("Member", "name"), Entity("Member", "address1"), Entity("Member", "address2"), Entity("Member", "address3"), Entity("Member", "passport")))), Relation(Entity("Class", "Stay"), RelationType("has"), Tree(Vector(Entity("Member", "stayId"), Entity("Member", "paymethod"), Entity("Member", "employee")))), Relation(Entity("Class", "ServiceType"), RelationType("has"), Tree(Vector(Entity("Member", "name"), Entity("Member", "price")))), Relation(Entity("Class", "Service"), RelationType("has"), Tree(Vector(Entity("Member", "serviceDate"), Entity("Member", "serviceCount")))), Relation(Entity("Class", "Room"), RelationType("has"), Tree(Vector(Entity("Member", "roomId"), Entity("Member", "bedCount"), Entity("Member", "roomType"), Entity("Member", "price1"), Entity("Member", "price2")))), Relation(Entity("Class", "RoomState"), RelationType("has"), Tree(Vector(Entity("Member", "date"), Entity("Member", "personCount"), Entity("Member", "state"))))))))

  val m6 = Vector(Relation(Entity("Section", "roomState"), RelationType("has"), Tree(Vector(StringAttribute("Title", "Room State Model"), Relation(Entity("State", "free"), RelationType("has"), Tree(Vector(Relation(Entity("Event", "book"), RelationType("precedes"), Tree(Vector(Entity("State", "booked")))), Relation(Entity("Event", "checkin"), RelationType("precedes"), Tree(Vector(Entity("State", "occupied")))), Relation(Entity("Event", "changeRoom"), RelationType("precedes"), Tree(Vector(Entity("State", "occupied")))), Relation(Entity("Event", "repair"), RelationType("precedes"), Tree(Vector(Entity("State", "repairing"))))))), Relation(Entity("State", "booked"), RelationType("has"), Tree(Vector(Relation(Entity("Event", "checkIn"), RelationType("precedes"), Tree(Vector(Entity("State", "occupied")))), Relation(Entity("Event", "cancel"), RelationType("precedes"), Tree(Vector(Entity("State", "free"))))))), Relation(Entity("State", "occupied"), RelationType("has"), Tree(Vector(Relation(Entity("Event", "checkout"), RelationType("precedes"), Tree(Vector(Entity("State", "free")))), Relation(Entity("Event", "changeRoom"), RelationType("precedes"), Tree(Vector(Entity("State", "free"))))))), Relation(Entity("State", "repairing"), RelationType("has"), Tree(Vector(Relation(Entity("Event", "done"), RelationType("precedes"), Tree(Vector(Entity("State", "free")))))))))))

  val m7 = Vector(StringAttribute("Title", "Test Model"), StringAttribute("Text", "This is a model to test html generation."), Relation(Entity("Feature", "topStuff"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "hejsan")))), Relation(Entity("Feature", "deepTopStuff"), RelationType("has"), Tree(Vector(Relation(Entity("Feature", "Gurka"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "hejsan"))))))), Relation(Entity("Section", "Context"), RelationType("has"), Tree(Vector(StringAttribute("Text", "This section describes the context of the system."), StringAttribute("Image", "context-diagram.svg"), Relation(Entity("Product", "hotelApp"), RelationType("implements"), Tree(Vector(Relation(Entity("Interface", "receptionUI"), RelationType("has"), Tree(Vector(Entity("Actor", "receptionist")))), Relation(Entity("Interface", "guestUI"), RelationType("has"), Tree(Vector(Entity("Actor", "guest")))), Relation(Entity("Interface", "phoneAPI"), RelationType("requires"), Tree(Vector(Entity("System", "telephony")))), Relation(Entity("Interface", "accountAPI"), RelationType("requires"), Tree(Vector(Entity("System", "accounting"))))))), Relation(Entity("Interface", "receptionUI"), RelationType("has"), Tree(Vector(Entity("Input", "booking"), Entity("Input", "checkOut"), Entity("Output", "serviceNote")))), Relation(Entity("Interface", "guestUI"), RelationType("has"), Tree(Vector(Entity("Output", "confirmation"), Entity("Output", "invoice"))))))), Relation(Entity("Section", "Quality"), RelationType("has"), Tree(Vector(StringAttribute("Text", "This section contains system-wide quality requirements."), Relation(Entity("Quality", "databaseCapacity"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "#guests < 10,000 growing 20% per year, #rooms < 1,000")))), Relation(Entity("Quality", "calendarAccuracy"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "Bookings shall be possible at least two years ahead.")))), Relation(Entity("Quality", "forecastPerformance"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "Product shall compute a room occupation forecast within ___ minutes. (Customer expects one minute.)")))), Relation(Entity("Quality", "taskUsability"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "Novice users shall perform tasks Q and R in 15 minutes. Experienced users shall perform tasks Q, R, S in 2 minutes.")))), Relation(Entity("Quality", "taskUsability"), RelationType("relatesTo"), Tree(Vector(Entity("Task", "Q"), Entity("Task", "R"), Entity("Task", "S")))), Relation(Entity("Quality", "peakLoadPerformance"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "Product shall be able to process 100 payment transactions per second in peak load."))))))))

  val m8 = Vector(Relation(Entity("Stakeholder", "a"), RelationType("has"), Tree(Vector(IntAttribute("Prio", 2), Relation(Entity("Req", "1"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 5)))), Relation(Entity("Req", "2"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 300)))), Relation(Entity("Req", "3"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 8)))), Relation(Entity("Req", "4"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 9)))), Relation(Entity("Req", "5"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 100)))), Relation(Entity("Req", "6"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 10)))), Relation(Entity("Req", "7"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 20))))))), Relation(Entity("Stakeholder", "b"), RelationType("has"), Tree(Vector(IntAttribute("Prio", 4), Relation(Entity("Req", "1"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 100)))), Relation(Entity("Req", "2"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 7)))), Relation(Entity("Req", "3"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 20)))), Relation(Entity("Req", "4"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 80)))), Relation(Entity("Req", "5"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 10)))), Relation(Entity("Req", "6"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 90)))), Relation(Entity("Req", "7"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 20))))))))

  val m9 = Vector(Entity("Req", "Musik"), Entity("Req", "Sport"), Entity("Req", "Skola"), Entity("Req", "Matlagning"), Entity("Req", "Dataspel"))

  val m10 = Vector(Relation(Entity("Task", "receptionWork"), RelationType("has"), Tree(Vector(Entity("Task", "booking"), Relation(Entity("Task", "checkIn"), RelationType("has"), Tree(Vector(StringAttribute("Why", "Guest wants room."), IntAttribute("Frequency", 3), StringAttribute("Spec", "Give guest a room, mark it as occupied and start account. Frequency scale is median number of check-ins/room/week. Trigger: A guest arrives. Critical: Group tour with 50 guests."), Entity("Task", "findRoom"), Relation(Entity("Task", "recordGuest"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "variants: a) Guest has booked in advance, b) No suitable room")))), Entity("Task", "deliverKey"))))))))

  val m11 = Vector(Relation(Entity("Quality", "mtts"), RelationType("has"), Tree(Vector(StringAttribute("Gist", "Mean time to startup"), StringAttribute("Spec", "Measured in milliseconds using Test startup"), Relation(Entity("Breakpoint", "Utility"), RelationType("has"), Tree(Vector(IntAttribute("Value", 4000)))), Relation(Entity("Breakpoint", "Differentiation"), RelationType("has"), Tree(Vector(IntAttribute("Value", 1500)))), Relation(Entity("Breakpoint", "Saturation"), RelationType("has"), Tree(Vector(IntAttribute("Value", 200)))), Relation(Entity("Target", "basic"), RelationType("has"), Tree(Vector(IntAttribute("Value", 2000), StringAttribute("Comment", "Probably possible with existing architecture.")))), Relation(Entity("Target", "strech"), RelationType("has"), Tree(Vector(IntAttribute("Value", 1100), StringAttribute("Comment", "Probably needs new architecture.")))), Relation(Entity("Barrier", "first"), RelationType("has"), Tree(Vector(IntAttribute("Value", 2100)))), Relation(Entity("Barrier", "second"), RelationType("has"), Tree(Vector(IntAttribute("Value", 1000)))), Relation(Entity("Product", "competitorX"), RelationType("has"), Tree(Vector(IntAttribute("Value", 2000)))), Relation(Entity("Product", "competitorY"), RelationType("has"), Tree(Vector(IntAttribute("Value", 3000))))))), Relation(Entity("Test", "startup"), RelationType("verifies"), Tree(Vector(Entity("Quality", "mtts")))), Relation(Entity("Test", "startup"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "Calculate average time in milliseconds of the startup time over 10  executions from start button is pressed to logon screen is shown."), Entity("Target", "stretch")))))

  val m12 = Vector(Relation(Entity("Quality", "dbCapacity"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "#guests < 10,000 growing 20% per year, #rooms < 1,000")))), Relation(Entity("Quality", "calendarAccuracy"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "Bookings shall be possible at least two years ahead.")))), Relation(Entity("Quality", "forecastPerformance"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "Product shall compute a room occupation forecast within ___ minutes. (Customer expects one minute.)")))), Relation(Entity("Quality", "taskTimeUsability "), RelationType("has"), Tree(Vector(StringAttribute("Spec", "Novice users shall perform tasks Q and R in 15 minutes. Experienced users tasks Q, R, S in 2 minutes.")))), Relation(Entity("Quality", "taskTimeUsability"), RelationType("requires"), Tree(Vector(Entity("Task", "Q"), Entity("Task", "R"), Entity("Task", "S")))), Relation(Entity("Quality", "peakLoadPerformance"), RelationType("has"), Tree(Vector(StringAttribute("Spec", "Product shall be able to process 100 payment transactions per second in peak load.")))))

  val m13 = Vector(Relation(Entity("Component", "appearance"), RelationType("has"), Tree(Vector(Relation(Entity("VariationPoint", "color"), RelationType("has"), Tree(Vector(IntAttribute("Min", 0), IntAttribute("Max", 2), Entity("Variant", "blue"), Entity("Variant", "red"), Entity("Variant", "green")))), Relation(Entity("VariationPoint", "shape"), RelationType("has"), Tree(Vector(IntAttribute("Min", 1), IntAttribute("Max", 1), Entity("Variant", "round"), Entity("Variant", "square")))), Relation(Entity("VariationPoint", "payment"), RelationType("has"), Tree(Vector(IntAttribute("Min", 1), IntAttribute("Max", 2), Entity("Variant", "cash"), Entity("Variant", "credit")))), Relation(Entity("VariationPoint", "payment"), RelationType("requires"), Tree(Vector(Entity("Variant", "cash")))), Relation(Entity("Variant", "round"), RelationType("excludes"), Tree(Vector(Entity("Variant", "red")))), Relation(Entity("Variant", "green"), RelationType("requires"), Tree(Vector(Entity("Variant", "square"))))))), Relation(Entity("Component", "appearance"), RelationType("requires"), Tree(Vector(Entity("VariationPoint", "shape")))), Relation(Entity("App", "free"), RelationType("requires"), Tree(Vector(Entity("Component", "appearance")))), Relation(Entity("App", "free"), RelationType("binds"), Tree(Vector(Relation(Entity("VariationPoint", "shape"), RelationType("binds"), Tree(Vector(Entity("Variant", "round"))))))), Relation(Entity("App", "premium"), RelationType("requires"), Tree(Vector(Entity("Component", "appearance")))), Relation(Entity("App", "premium"), RelationType("binds"), Tree(Vector(Relation(Entity("VariationPoint", "color"), RelationType("binds"), Tree(Vector(Entity("Variant", "red"), Entity("Variant", "green")))), Relation(Entity("VariationPoint", "shape"), RelationType("binds"), Tree(Vector(Entity("Variant", "round"), Entity("Variant", "square")))), Relation(Entity("VariationPoint", "payment"), RelationType("binds"), Tree(Vector(Entity("Variant", "cash"))))))))

  val m14 = Vector(Relation(Entity("Stakeholder", "X"), RelationType("has"), Tree(Vector(IntAttribute("Prio", 1), Relation(Entity("Feature", "1"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 4)))), Relation(Entity("Feature", "2"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 2)))), Relation(Entity("Feature", "3"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 1))))))), Relation(Entity("Stakeholder", "Y"), RelationType("has"), Tree(Vector(IntAttribute("Prio", 2), Relation(Entity("Feature", "1"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 2)))), Relation(Entity("Feature", "2"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 1)))), Relation(Entity("Feature", "3"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 1))))))), Relation(Entity("Release", "A"), RelationType("precedes"), Tree(Vector(Entity("Release", "B")))), Relation(Entity("Resource", "dev"), RelationType("has"), Tree(Vector(Relation(Entity("Feature", "1"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 10)))), Relation(Entity("Feature", "2"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 70)))), Relation(Entity("Feature", "3"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 40)))), Relation(Entity("Release", "A"), RelationType("has"), Tree(Vector(IntAttribute("Capacity", 100)))), Relation(Entity("Release", "B"), RelationType("has"), Tree(Vector(IntAttribute("Capacity", 100))))))), Relation(Entity("Resource", "test"), RelationType("has"), Tree(Vector(Relation(Entity("Feature", "1"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 40)))), Relation(Entity("Feature", "2"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 10)))), Relation(Entity("Feature", "3"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 70)))), Relation(Entity("Release", "A"), RelationType("has"), Tree(Vector(IntAttribute("Capacity", 100)))), Relation(Entity("Release", "B"), RelationType("has"), Tree(Vector(IntAttribute("Capacity", 100))))))), Relation(Entity("Feature", "3"), RelationType("precedes"), Tree(Vector(Entity("Feature", "1")))))

  val m15 = Vector(Relation(Entity("Feature", "exportHtml"), RelationType("has"), Tree(Vector(StringAttribute("Gist", "Export model to HTML, with special treatment of Section and Image.")))), Relation(Entity("Feature", "exportGraphViz"), RelationType("has"), Tree(Vector(StringAttribute("Gist", "Export model to graph for visualization in GraphViz.")))), Relation(Entity("Feature", "exportTabular"), RelationType("has"), Tree(Vector(StringAttribute("Gist", "Export model to table format for edit in spreadsheet apps.")))), Relation(Entity("Feature", "exportLatex"), RelationType("has"), Tree(Vector(StringAttribute("Gist", "Export model to Latex, with special treatment of Section.")))), Relation(Entity("Feature", "exportContextDiagramSvg"), RelationType("has"), Tree(Vector(StringAttribute("Gist", "Solve release planning problems.")))), Relation(Entity("Feature", "syntaxColoring"), RelationType("has"), Tree(Vector(StringAttribute("Gist", "Syntax colored editing of models.")))), Relation(Entity("Feature", "releasePlanning"), RelationType("has"), Tree(Vector(StringAttribute("Gist", "Constraint-solving support and gui for release planning.")))), Relation(Entity("Feature", "autoCompletion"), RelationType("has"), Tree(Vector(StringAttribute("Gist", "Auto-completion of entity, attribute and relation types.")))), Relation(Entity("Feature", "autoSave"), RelationType("has"), Tree(Vector(StringAttribute("Gist", "Save a model automatically after each update.")))), Relation(Entity("Resource", "TeamA"), RelationType("has"), Tree(Vector(Relation(Entity("Feature", "exportHtml"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 9)))), Relation(Entity("Feature", "exportGraphViz"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 7)))), Relation(Entity("Feature", "exportTabular"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 3)))), Relation(Entity("Feature", "exportLatex"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 6)))), Relation(Entity("Feature", "exportContextDiagramSvg"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 3)))), Relation(Entity("Feature", "syntaxColoring"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 6)))), Relation(Entity("Feature", "autoCompletion"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 3)))), Relation(Entity("Feature", "releasePlanning"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 4)))), Relation(Entity("Feature", "autoSave"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 6)))), Relation(Entity("Release", "March"), RelationType("has"), Tree(Vector(IntAttribute("Capacity", 20)))), Relation(Entity("Release", "July"), RelationType("has"), Tree(Vector(IntAttribute("Capacity", 15)))), Relation(Entity("Release", "later"), RelationType("has"), Tree(Vector(IntAttribute("Capacity", 1000))))))), Relation(Entity("Resource", "TeamB"), RelationType("has"), Tree(Vector(Relation(Entity("Feature", "exportHtml"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 2)))), Relation(Entity("Feature", "exportGraphViz"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 8)))), Relation(Entity("Feature", "exportTabular"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 9)))), Relation(Entity("Feature", "exportLatex"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 4)))), Relation(Entity("Feature", "exportContextDiagramSvg"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 4)))), Relation(Entity("Feature", "syntaxColoring"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 2)))), Relation(Entity("Feature", "autoCompletion"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 3)))), Relation(Entity("Feature", "releasePlanning"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 5)))), Relation(Entity("Feature", "autoSave"), RelationType("has"), Tree(Vector(IntAttribute("Cost", 7)))), Relation(Entity("Release", "March"), RelationType("has"), Tree(Vector(IntAttribute("Capacity", 15)))), Relation(Entity("Release", "July"), RelationType("has"), Tree(Vector(IntAttribute("Capacity", 15)))), Relation(Entity("Release", "later"), RelationType("has"), Tree(Vector(IntAttribute("Capacity", 1000))))))), Relation(Entity("Release", "March"), RelationType("has"), Tree(Vector(IntAttribute("Order", 1)))), Relation(Entity("Release", "July"), RelationType("has"), Tree(Vector(IntAttribute("Order", 2)))), Relation(Entity("Release", "later"), RelationType("has"), Tree(Vector(IntAttribute("Order", 3)))), Relation(Entity("Stakeholder", "Ada"), RelationType("has"), Tree(Vector(IntAttribute("Prio", 1), Relation(Entity("Feature", "exportHtml"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 10)))), Relation(Entity("Feature", "exportGraphViz"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 10)))), Relation(Entity("Feature", "exportTabular"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 10)))), Relation(Entity("Feature", "exportLatex"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 7)))), Relation(Entity("Feature", "exportContextDiagramSvg"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 6)))), Relation(Entity("Feature", "syntaxColoring"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 3)))), Relation(Entity("Feature", "releasePlanning"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 4)))), Relation(Entity("Feature", "autoCompletion"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 7)))), Relation(Entity("Feature", "autoSave"), RelationType("has"), Tree(Vector(IntAttribute("Benefit", 9))))))))

}
