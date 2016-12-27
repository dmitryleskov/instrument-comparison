/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment

import java.io.FileNotFoundException
import java.time.temporal.ChronoUnit.MONTHS

import investment.SimulationModel.InstalmentRuleID.SalaryPercentage
import investment.SimulationModel.{InstalmentRuleID, StrategyID}
import investment.instruments.Instrument
import investment.util.Storage
import org.xml.sax.SAXParseException

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.Bindings
import scalafx.beans.property.{IntegerProperty, ReadOnlyStringProperty, StringProperty}
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, HBox, StackPane, VBox}
import scalafx.scene.text.Font
import scalafx.scene.{Node, Scene}
import scalafx.util.StringConverter

object Main extends JFXApp {

  def updateChart: Unit = {
    val allTimeStats = SimulationModel.statistics.value.allTimeStats

    // Shift serial numbers (1-based) so that Januaries fall on multiples of 12
    val offset = allTimeStats.start.getMonthValue - 2

    val data0 = ObservableBuffer(allTimeStats.inflation map {
      case (ym, v) => XYChart.Data[Number, Number](allTimeStats.start.until(ym, MONTHS) + 1 + offset, v) })
    val series0 = XYChart.Series[Number, Number]("Inflation", data0)

    val data1 = ObservableBuffer(allTimeStats.aggregateInvestment map {
      case (ym, v) => XYChart.Data[Number, Number](allTimeStats.start.until(ym, MONTHS) + 1 + offset, v) })
    val series1 = XYChart.Series[Number, Number]("Investment", data1)

    val data2 = ObservableBuffer(allTimeStats.portfolioValuations map {
      case (ym, v) => XYChart.Data[Number, Number](allTimeStats.start.until(ym, MONTHS) +1 + offset, v) })
    val series2 = XYChart.Series[Number, Number]("Portfolio", data2)

    lineChart.getData.clear()
    lineChart.getData.add(series0)
    lineChart.getData.add(series1)
    lineChart.getData.add(series2)
  }

  private val _summary = StringProperty("")
  def summary: ReadOnlyStringProperty = _summary

  def updateStats: Unit = {
    val allTime = SimulationModel.statistics.value.allTimeStats
    _summary.value =
      "Period: " + allTime.start + " - " + allTime.start.plusMonths(allTime.duration - 1) + " (" + allTime.duration + " months)\n" +
        "Last instalment: " + allTime.instalments.last._2.formatted("%.2f") + "\n" +
        //        snapshots.last.portfolio + "\n" +
        "Portfolio Value: " + allTime.portfolioValuations.last._2.formatted("%.2f") + "\n" +
        "Total Investment: " + allTime.totalInvestment.formatted("%.2f") + "\n" +
        "Total Income: " + allTime.totalIncome.formatted("%.2f") + "\n" +
        "Capital Gain: " + (allTime.portfolioValuations.last._2 - allTime.totalInvestment - allTime.totalIncome).formatted("%.2f") + "\n" +
        "Last 12M Income: " + allTime.last12MonthsIncome.formatted("%.2f") + "\n" +
        "Return: " + (allTime.returnOnInvestment0 * 100).formatted("%.1f%%") + "\n" +
        "Inflation-adjusted Return: " + (allTime.returnOnInvestment * 100).formatted("%.1f%%") + "\n" +
        "Absolute drawdown: " + allTime.absoluteDrawdown0 + "\n" +
        "Absolute drawdown (inflation adjusted): " + allTime.absoluteDrawdown + "\n" +
        "Maximum drawdown: " + allTime.maximumDrawdown0 + "\n" +
        "Relative drawdown: " + allTime.relativeDrawdown0 + "\n\n" +
        SimulationModel.statistics.value.statsByInterval
  }

  def intField(_maxWidth: Int, bindTo: IntegerProperty): TextField =
    new TextField {
      maxWidth = _maxWidth
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
        new Button("Edit Allocation") {
          onAction = (e: ActionEvent) => {rootNode.children = alllocationEditor}
        },
        new Label("Initial Amount"),
        intField(100, SimulationModel.initialAmount),
        new Label("Instalments"),
        new ChoiceBox[InstalmentRuleID] {
          items = ObservableBuffer(InstalmentRuleID.values)
          value <==> SimulationModel.instalmentRuleId
        },
        new StackPane {
          alignment = javafx.geometry.Pos.TOP_LEFT
          children = List(
            new VBox {
              visible <== SimulationModel.instalmentRuleId =!= SalaryPercentage
              children = List(
                new Label("Initial Instalment") {
                  visible <== SimulationModel.instalmentRuleId =!= SalaryPercentage
                }, {
                  val ii = intField(100, SimulationModel.initialInstalment)
                  ii.disable <== SimulationModel.instalmentRuleId === SalaryPercentage
                  ii
                })
            }, new HBox {
              alignment = javafx.geometry.Pos.CENTER_LEFT
              spacing = 5
              visible <== SimulationModel.instalmentRuleId === SalaryPercentage
              children = List(
                {
                  val sp = intField(40, SimulationModel.salaryPercentage)
                  sp.alignment = javafx.geometry.Pos.CENTER_RIGHT
                  sp.disable <== SimulationModel.instalmentRuleId =!= SalaryPercentage
                  sp
                },
                new Label("%")
              )
            }
          )
        },
        new Label("Strategy"),
        new ChoiceBox[StrategyID] {
          items = ObservableBuffer(StrategyID.values)
          value <==> SimulationModel.strategyId
        }
      )
    }

  val xAxis = new NumberAxis {
    tickLabelFormatter = new StringConverter[Number] {
      override def fromString(s: String) = 0
      override def toString(n: Number) = {
        if(SimulationModel.statistics.value != null)
          SimulationModel.statistics.value.start.plusMonths(n.intValue).getYear.toString
        else ""
      }
    }
    autoRanging = false
    tickUnit = 12
    minorTickCount = 4
    lowerBound = 0
    upperBound <== (SimulationModel.maxYear - SimulationModel.minYear + 1) * 12
  }
  val yAxis = new NumberAxis
  val lineChart = new LineChart(xAxis, yAxis) {
    title = "Performance"
  }

  val tabpane = new TabPane {
    tabs = Seq(
      new Tab {
        text = "Chart"
        closable = false
        content = lineChart
      },
      new Tab {
        text = "Stats"
        closable = false
        content = new TextArea {
          editable = false
          font = new Font("Arial", 16)
          text <== summary
        }
      }
    )
  }

  val chart = new BorderPane {
    left = addVBox
    center = tabpane
  }

  val rootNode = new StackPane {
    children = chart
  }


  stage = new PrimaryStage {
    title = "Instrument Comparison"
    scene = new Scene(800, 600) {
      root = rootNode
    }
  }

  SimulationModel.statistics.onChange(updateChart)
  SimulationModel.statistics.onChange(updateStats)

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
  (settings \ "initialAmount").headOption.collect { case node =>
    SimulationModel.initialAmount.value = node.text.toInt
  }
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
    (settings \ "allocation").headOption.collect { case node: xml.NodeSeq =>
      Storage.fromXML[List[(Instrument, Int)]](node) match {
        case Some(allocation) => println("Settings file found, allocation loaded: " + allocation)
          SimulationModel.allocationProperty.value = new FixedAllocation((for ((i, w) <- allocation) yield (i, w.toDouble)).toMap)
          allocation
        case None => println("Settings file found, but allocation not loaded"); List.empty
      }
    }

  val allocationModel = new AllocationModel(allocation.getOrElse(List.empty))
  val alllocationEditor = AllocationEditor.init(allocationModel, () => {
    SimulationModel.allocationProperty.value = new FixedAllocation(allocationModel.get.toMap)
    rootNode.children = chart
  })

  override def stopApp = {
    val x = <settings>
      {if (allocationModel.length > 0) Storage.toXML(allocationModel.getWeights)}
      <initialAmount>{SimulationModel.initialAmount.value}</initialAmount>
      <initialInstalment>{SimulationModel.initialInstalment.value}</initialInstalment>
      {Storage.toXML(SimulationModel.instalmentRuleId.value)}
      {Storage.toXML(SimulationModel.strategyId.value)}
    </settings>
    xml.XML.save(".portfolio", x.head, "utf-8")
  }
}

