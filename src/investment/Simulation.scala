package investment

import java.io.FileNotFoundException
import java.time.Month

import investment.SimulationModel.{InstalmentCurrencyID, InstalmentRuleID, StrategyID}
import org.xml.sax.SAXParseException

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.Bindings
import scalafx.beans.property.{DoubleProperty, IntegerProperty}
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, HBox, StackPane, VBox}
import scalafx.scene.{Node, Scene}
import scalafx.util.StringConverter

object Simulation extends JFXApp {

  def updateChart: Unit = {
    // Shift serial numbers (1-based) so that Januaries fall on multiples of 12
    val offset = SimulationModel.snapshots(0).ym.getMonthValue - 2

    val investmentRT =
      SimulationModel.snapshots
        .map(x => (x.serial, x.instalment))
        .scanLeft((0, SimulationModel.initialAmount.value.toDouble))({case ((_, prev), (month, amount)) => (month,prev + amount)})
        .tail

    val data0 = ObservableBuffer(SimulationModel.inflation map { case Snapshot(serial, _, _, _, value) => XYChart.Data[Number, Number](serial + offset, value) })
    val series0 = XYChart.Series[Number, Number]("Inflation", data0)

    val data1 = ObservableBuffer(investmentRT map { case (x, y) => XYChart.Data[Number, Number](x + offset, y) })
    val series1 = XYChart.Series[Number, Number]("Investment", data1)

    val data2 = ObservableBuffer(SimulationModel.portfolioValues map { case (x, y) => XYChart.Data[Number, Number](x + offset, y) })
    val series2 = XYChart.Series[Number, Number]("Portfolio", data2)

    lineChart.getData.clear()
    lineChart.getData.add(series0)
    lineChart.getData.add(series1)
    lineChart.getData.add(series2)
  }

  val settings = try {
    xml.XML.load(".portfolio")
  } catch {
    case ex: FileNotFoundException =>
      println("Settings file not found")
      xml.NodeSeq.Empty
    case ex: SAXParseException =>
      println("Settings file contents is not well-formed XML: " + ex.getMessage)
      xml.NodeSeq.Empty
  }
  println(settings)
  println(settings \ "portfolio")

  println(settings \ "initialAmount")
  (settings \ "initialAmount").headOption.collect { case node =>
    SimulationModel.initialAmount.value = node.text.toInt
  }
  println(settings \ "initialInstalment")
  (settings \ "initialInstalment").headOption.collect { case node =>
    SimulationModel.initialInstalment.value = node.text.toInt
  }
  (settings \ "instalmentRuleID").headOption.collect { case node =>
    Storage.fromXML[InstalmentRuleID](node) match {
      case Some(irid) => println("Instalment Rule ID loaded: " + irid)
        SimulationModel.instalmentRuleId.value = irid
      case None => println("Instalment Rule ID not loaded")
    }
  }
  (settings \ "strategyID").headOption.collect { case node =>
    Storage.fromXML[StrategyID](node) match {
      case Some(sid) => println("Strategy ID loaded: " + sid)
        SimulationModel.strategyId.value = sid
      case None => println("Strategy ID not loaded")
    }
  }
  val allocation =
    (settings \ "portfolio").headOption.collect { case node: xml.NodeSeq =>
      Storage.fromXML[List[(Instrument, Int)]](node) match {
        case Some(portfolio) => println("Settings file found, portfolio loaded: " + portfolio)
          SimulationModel.allocationProperty.value = new FixedAllocation((for ((i, w) <- portfolio) yield (i, w.toDouble)).toMap)
          portfolio
        case None => println("Settings file found, but portfolio not loaded"); List.empty
      }
    }

  val portfolioModel = new PortfolioModel(allocation.getOrElse(List.empty))
  val portfolioEditor = PortfolioEditor.init(portfolioModel, () => {
    SimulationModel.allocationProperty.value = new FixedAllocation(portfolioModel.get.toMap)
    rootNode.children = chart
  })

  SimulationModel.portfolioValues.onChange(updateChart)
  SimulationModel.inflation.onChange(updateChart)

  def intField(_maxWidth: Int, bindTo: IntegerProperty): TextField =
    new TextField {
      maxWidth = 100
      textFormatter = new TextFormatter(
        javafx.scene.control.TextFormatter.IDENTITY_STRING_CONVERTER,
        bindTo.value.toString, { change => if (change.text.matches("[0-9]*")) change else null }) {}
      val binding = Bindings.createIntegerBinding(
        () => {if (text.value.isEmpty) 0 else text.value.toInt},
        text)
      focused.onChange({ (_, _, newVal) => if (!newVal)
        bindTo.value = binding.intValue
      })
    }

  def addVBox: Node =
    new VBox {
      padding = Insets(15)
      spacing = 10
      children = List(
        new Button("Edit Portfolio") {
          onAction = (e: ActionEvent) => {rootNode.children = portfolioEditor}
        },
        new Label("Initial Amount"),
        intField(100, SimulationModel.initialAmount),
        new Label("Monthly Instalment"),
        new HBox {
          children = List(
            intField(100, SimulationModel.initialInstalment),
            new ChoiceBox[InstalmentCurrencyID] {
              items = ObservableBuffer(InstalmentCurrencyID.values)
              value <==> SimulationModel.instalmentCurrencyId
            }
          )
        },
        new ChoiceBox[InstalmentRuleID] {
          items = ObservableBuffer(InstalmentRuleID.values)
          value <==> SimulationModel.instalmentRuleId
        },
        new ChoiceBox[StrategyID] {
          items = ObservableBuffer(StrategyID.values)
          value <==> SimulationModel.strategyId
        },
        new Separator(),
        new TextArea {
          maxWidth = 150
          editable = false
          text <== SimulationModel.summary
        }
      )
    }

  val xAxis = new NumberAxis {
    tickLabelFormatter = new StringConverter[Number] {
      override def fromString(s: String) = 0
      override def toString(n: Number) = {
        if (!SimulationModel.snapshots.isEmpty)
          SimulationModel.snapshots(0).ym.plusMonths(n.intValue).getYear.toString
        else ""
      }
    }
    autoRanging = false
    tickUnit = 12
    minorTickCount = 4
    lowerBound = 0
    upperBound <== (SimulationModel.length / 12 + 1) * 12
  }
  val yAxis = new NumberAxis
  val lineChart = LineChart(xAxis, yAxis) 
  lineChart.title = "Portfolio Performance"

  val chart = new BorderPane {
    left = addVBox
    center = lineChart
  }

  val rootNode = new StackPane {
    children = chart
  }

  stage = new PrimaryStage {
    title = "Portfolio Performance"
    scene = new Scene(800, 600) {
      root = rootNode
    }
  }

  override def stopApp = {
    val x = <settings>
      {if (portfolioModel.length > 0) Storage.toXML(portfolioModel.getWeights)}
      <initialAmount>{SimulationModel.initialAmount.value}</initialAmount>
      <initialInstalment>{SimulationModel.initialInstalment.value}</initialInstalment>
      {Storage.toXML(SimulationModel.instalmentRuleId.value)}
      {Storage.toXML(SimulationModel.strategyId.value)}
    </settings>
    xml.XML.save(".portfolio", x.head, "utf-8")
  }
}

