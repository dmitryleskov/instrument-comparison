package investment

import java.time.YearMonth
import java.time.temporal.ChronoUnit

import investment.SimulationModel.StrategyID
import investment.SimulationModel.StrategyID.{BalanceGradually, RebalanceMonthly, Split}

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, HBox, StackPane, VBox}
import scalafx.scene.{Node, Scene}

object SimulationModel {
  val portfolioModel = new PortfolioModel
  val results = new ObservableBuffer[(Int, Double, investment.Portfolio)]()
  val portfolioValues = new ObservableBuffer[(Int, Double)]

  sealed abstract class StrategyID
  object StrategyID {
    case object Split extends StrategyID {override def toString() = "Simple Split"}
    case object BalanceGradually extends StrategyID {override def toString() = "Balance Gradually"}
    case object RebalanceMonthly extends StrategyID {override def toString() = "Rebalance Monthly"}
    val values = Seq(Split, BalanceGradually, RebalanceMonthly)
  }

  val strategyId = ObjectProperty[StrategyID](this, "strategy")
  strategyId.onChange(updateResults)

  def updateResults() = {
    val portfolio = portfolioModel.get
    if (portfolio.isEmpty || strategyId.value == null) {
      results.clear()
      portfolioValues.clear()
    } else {
      val start = (portfolio map (_._1.startDate)).max
      val end = (portfolio map (_._1.endDate)).min
      val allocation = new FixedAllocation(portfolio.toMap)
      val strategy = strategyId.value match {
        case Split => new Split(allocation)
        case BalanceGradually => new BalanceGradually(allocation)
        case RebalanceMonthly => new RebalanceMonthly(allocation)
      }

      val instalmentRule =
        new FixedAmount(start, 1000.0)
//        new FixedUSDAmount(start, 10.0)
//        new InflationAdjusted(start, 1000.0)

      val sim = new Simulator(
        allocation,
        instalmentRule,
        strategy)

      results.setAll(sim.simulate(start, start.until(end, ChronoUnit.MONTHS).toInt): _*)
      def portfolioValue(ym: YearMonth, portfolio: Portfolio): Double = {
        (for(Position(instrument, amount) <- portfolio) yield instrument.price(ym) * amount).sum
      }
      portfolioValues.setAll(results map (x => (x._1, portfolioValue(start.plusMonths(x._1 - 1), x._3))))
    }
    ()
  }
  portfolioModel.onChange(c => updateResults)
}

object Simulation extends JFXApp {

  def updateChart: Unit = {
    val investmentRT =
      SimulationModel.results
        .map(x => (x._1, x._2))
        .scanLeft((0, 0.0))({case ((_, prev), (month, amount)) => (month,prev + amount)})
        .tail

//    val data0 = ObservableBuffer(Inflation.rates map { case (x, y) => XYChart.Data[Number, Number](x, y) })
//    val series0 = XYChart.Series[Number, Number]("Investment", data0)

    val data1 = ObservableBuffer(investmentRT map { case (x, y) => XYChart.Data[Number, Number](x, y) })
    val series1 = XYChart.Series[Number, Number]("Investment", data1)

    val data2 = ObservableBuffer(SimulationModel.portfolioValues map { case (x, y) => XYChart.Data[Number, Number](x, y) })
    val series2 = XYChart.Series[Number, Number]("Portfolio", data2)

    lineChart.getData.clear()
    lineChart.getData.add(series1)
    lineChart.getData.add(series2)
  }

  val portfolioEditor = PortfolioEditor.init(SimulationModel.portfolioModel, () => {
    rootNode.children = chart
  })

  SimulationModel.portfolioValues.onChange(updateChart)

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
            },
            new ChoiceBox[String] {
              items = ObservableBuffer(Seq("RUB", "USD"))
            }
          )
        },
        new ChoiceBox[String] {
          items = ObservableBuffer(Seq("Constant", "Fixed % increase", "Inflation adjusted"))
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
}

