package investment

import investment.SimulationModel.{InstalmentCurrencyID, InstalmentRuleID, StrategyID}

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.Bindings
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, HBox, StackPane, VBox}
import scalafx.scene.{Node, Scene}

object Simulation extends JFXApp {

  def updateChart: Unit = {
    val investmentRT =
      SimulationModel.results
        .map(x => (x.serial, x.instalment))
        .scanLeft((0, 0.0))({case ((_, prev), (month, amount)) => (month,prev + amount)})
        .tail

    println(SimulationModel.inflation.length)
    val data0 = ObservableBuffer(SimulationModel.inflation map { case Snapshot(serial, _, _, _, value) => XYChart.Data[Number, Number](serial, value) })
    val series0 = XYChart.Series[Number, Number]("Inflation", data0)

    val data1 = ObservableBuffer(investmentRT map { case (x, y) => XYChart.Data[Number, Number](x, y) })
    val series1 = XYChart.Series[Number, Number]("Investment", data1)

    val data2 = ObservableBuffer(SimulationModel.portfolioValues map { case (x, y) => XYChart.Data[Number, Number](x, y) })
    val series2 = XYChart.Series[Number, Number]("Portfolio", data2)

    lineChart.getData.clear()
    lineChart.getData.add(series0)
    lineChart.getData.add(series1)
    lineChart.getData.add(series2)
  }

  //println(SessionManager.t)

  val portfolioEditor = PortfolioEditor.init(SimulationModel.portfolioModel, () => {
    rootNode.children = chart
  })

  SimulationModel.portfolioValues.onChange(updateChart)
  SimulationModel.inflation.onChange(updateChart)

  def addVBox: Node =
    new VBox {
      padding = Insets(15)
      spacing = 10
      children = List(
        new Button("Edit Portfolio") {
          onAction = (e: ActionEvent) => {rootNode.children = portfolioEditor}
        },
        new Label("Monthly Instalment"),
        new HBox {
          children = List(
            new TextField {
              maxWidth = 100
              val filter = { change: TextFormatter.Change =>
                if (change.text.matches("[0-9]*")) change else null
              }
              textFormatter = new TextFormatter(javafx.scene.control.TextFormatter.IDENTITY_STRING_CONVERTER,
              "1000", filter) {

              }
              //SimulationModel.initialInstalment <== text.value.toInt
              val initialInstalmentBinding = Bindings.createIntegerBinding (
                () => {println("BINDING"); text.value.toInt},
                text
              )
              focused.onChange({(_, _, newVal) => if (!newVal)
                SimulationModel.initialInstalment.value = initialInstalmentBinding.intValue
              })


            },
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
        }
      )
    }

  // Defining the axes
  val xAxis = new NumberAxis
  xAxis.label = "Number of Month"
  val yAxis = new NumberAxis

  // Creating the chart
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
    val x = Storage.toXML(SimulationModel.portfolioModel.getWeights)
    xml.XML.save(".portfolio", x.head)
  }
}

