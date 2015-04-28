package investment

import java.time.Year

import scala.concurrent.duration.span
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import javafx.beans.binding.ObjectBinding
import scalafx.geometry.Insets
import scalafx.scene.control.{ChoiceBox, ComboBox}
import scalafx.scene.layout.{VBox, HBox, BorderPane}
import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.chart.{CategoryAxis, LineChart, NumberAxis, XYChart}
import scalafx.collections.ObservableBuffer

object Simulation extends JFXApp {
  val gmkn = Stock("GMKN")
  val lkoh = Stock("LKOH")
  val mgnt = Stock("MGNT")
  val mtss = Stock("MTSS")
  val rosn = Stock("ROSN")
  val sber = Stock("SBER")
  val sberp = Stock("SBERP")
  val sngsp = Stock("SNGSP")
  val vtbr = Stock("VTBR")
  val usd = new CashUSD
  val deposit = new Deposit(0.10)
  val depositUSD = new DepositUSD(0.03)

  // Defining the axes
  val xAxis = new NumberAxis
  xAxis.label = "Number of Month"
  val yAxis = new NumberAxis

  // Creating the chart
  val lineChart = LineChart(xAxis, yAxis)
  lineChart.title = "Portfolio Performance"

  def updateChart(instrument: Instrument) = {
    //val allocationDesc = List((sber, 1.0), (gmkn, 1.0), (sngsp, 1.0), (lkoh, 1.0), (mtss, 1.0))
    val allocationDesc = List((instrument, 1.0))
    val start = (allocationDesc map (_._1.startDate)).max
    val allocation = new FixedAllocation(allocationDesc.toMap)
    val strategy =
    // new Split(allocation)
      new BalanceGradually(allocation)
    // new RebalanceMonthly(allocation)

    val sim = new Simulator(
      allocation,
      // new InflationAdjusted(start.plusMonths(d), 1000.0),
      // new AnnualIncrease(start.plusMonths(d), 1000.0, 1.05),
      new FixedAmount(start, 1000.0),
      strategy)
    //      new AllToFirst)
    val snapshots = sim.simulate(start, 120)

    val investmentRT =
      snapshots.map(x => (x._1, x._2))
        .scanLeft((0, 0.0))({case ((_, prev), (month, amount)) => (month,prev + amount)})
        .tail

    val data1 = ObservableBuffer(investmentRT map { case (x, y) => XYChart.Data[Number, Number](x, y) })
    val series1 = XYChart.Series[Number, Number]("Investment", data1)

    val portfolioValues = snapshots map (x => (x._1, strategy.portfolioValue(start.plusMonths(x._1 - 1), x._3)))
    val data2 = ObservableBuffer(portfolioValues map { case (x, y) => XYChart.Data[Number, Number](x, y) })
    val series2 = XYChart.Series[Number, Number]("Portfolio", data2)

    lineChart.getData.clear()
    lineChart.getData.add(series1)
    lineChart.getData.add(series2)
  }
  def addVBox =
    new VBox {
      padding = Insets(15)
      spacing = 10
      style = "-fx-background-color: #336699;"
      children = List(
        new ChoiceBox[Instrument] {
          maxWidth = 200
          items = ObservableBuffer(lkoh, sber, depositUSD)
          value.onChange((_,_,bla) => updateChart(bla))
        }
      )
    }

  val border = new BorderPane {
    left = addVBox
    center = lineChart
  }

  stage = new PrimaryStage {
    title = "Portfolio Performance"
    scene = new Scene(800, 600) {
      root = border
    }
  }
}

