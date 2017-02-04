/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment

import java.io.FileNotFoundException
import java.time.YearMonth
import java.time.temporal.ChronoUnit.MONTHS

import investment.AllocationModel.DateRange
import investment.Main.DisplayStats.{DisplayItem1, DisplayItem2}
import investment.SimulationModel.InstalmentRuleID.SalaryPercentage
import investment.SimulationModel.{InstalmentRuleID, StrategyID}
import investment.Statistics.{AbsoluteDrawdown, BWML, Measure, RelativeDrawdown, Results}
import investment.instruments.Instrument
import investment.util.Storage
import org.xml.sax.SAXParseException

import scala.collection.immutable.IndexedSeq
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.Bindings
import scalafx.beans.property._
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.{Node, Scene}
import scalafx.stage.Screen
import scalafx.util.StringConverter

object Main extends JFXApp {
  val adjustForInflation = BooleanProperty(false)
  adjustForInflation.onChange(updateChart)

  def updateChart: Unit = {
    lineChart.getData.clear()
    if (SimulationModel.statistics.value != null) {
      val allTimeStats = SimulationModel.statistics.value.allTime

      // Shift serial numbers (1-based) so that Januaries fall on multiples of 12
      val offset = allTimeStats.start.getMonthValue - 2

      def series(name: String, source: Seq[(YearMonth, Double)]) =
        XYChart.Series[Number, Number](name, ObservableBuffer(source map {
          case (ym, v) => XYChart.Data[Number, Number](allTimeStats.start.until(ym, MONTHS) + 1 + offset, v)
        }))

      if (adjustForInflation.value) {
        lineChart.getData.add(series("Investment", allTimeStats.aggregateInvestment))
        lineChart.getData.add(series("Inflation", allTimeStats.inflation))
        lineChart.getData.add(series("Assets Value", allTimeStats.portfolioValues))
      } else {
        lineChart.getData.add(series("Investment", allTimeStats.aggregateInvestment0))
        lineChart.getData.add(series("Inflation", allTimeStats.inflation0))
        lineChart.getData.add(series("Assets Value", allTimeStats.portfolioValues0))
      }
    }
  }

  object DisplayStats {
    abstract class DisplayItem(val name: String)
    case class DisplayItem1(override val name: String, private val getValue: Statistics => String) extends DisplayItem(name) {
      private val wrapper = ReadOnlyStringWrapper("")
      val property: ReadOnlyStringProperty = wrapper.readOnlyProperty
      wrapper <== Bindings.createStringBinding(
        () => if (SimulationModel.statistics.value != null) getValue(SimulationModel.statistics.value) else "N/A",
        SimulationModel.statistics)
    }

    case class DisplayItem2(override val name: String,
                       private val getNominal: Statistics => String,
                       private val getDeflated: Statistics => String) extends DisplayItem(name) {
      private val nominalWrapper = ReadOnlyStringWrapper("")
      val nominalValue: ReadOnlyStringProperty = nominalWrapper.readOnlyProperty
      nominalWrapper <== Bindings.createStringBinding(
        () => if (SimulationModel.statistics.value != null) getNominal(SimulationModel.statistics.value) else "N/A",
        SimulationModel.statistics)
      private val deflatedWrapper = ReadOnlyStringWrapper("")
      val deflatedValue: ReadOnlyStringProperty = deflatedWrapper.readOnlyProperty
      deflatedWrapper <== Bindings.createStringBinding(
        () => if (SimulationModel.statistics.value != null) getDeflated(SimulationModel.statistics.value) else "N/A",
        SimulationModel.statistics)
    }

    val items: IndexedSeq[DisplayItem] = IndexedSeq(
      DisplayItem1("Period", (stats) => stats.allTime.start + " - " + stats.allTime.start.plusMonths(stats.allTime.duration - 1) + " (" + stats.allTime.duration + " months)"),
      DisplayItem1("Nominal Investment", (stats) => stats.allTime.totalInvestment0.formatted("%.2f")),
      DisplayItem1("Investment Value", (stats) => stats.allTime.inflation.last._2.formatted("%.2f")),
      DisplayItem1("Portfolio Value", (stats) => stats.allTime.portfolioValues0.last._2.formatted("%.2f")),
      DisplayItem2("Return", (stats) => (stats.allTime.returnOnInvestment0 * 100).formatted("%.1f%%"), (stats) => (stats.allTime.returnOnInvestment * 100).formatted("%.1f%%")),
      DisplayItem2("Absolute drawdown", (stats) => stats.allTime.absoluteDrawdown0.toString, (stats) => stats.allTime.absoluteDrawdown.toString),
      DisplayItem2("Maximum drawdown", (stats) => stats.allTime.maximumDrawdown0.toString, (stats) => stats.allTime.maximumDrawdown.toString),
      DisplayItem2("Relative drawdown", (stats) => stats.allTime.relativeDrawdown0.toString, (stats) => stats.allTime.relativeDrawdown.toString),
      DisplayItem1("Total Income", (stats) => stats.allTime.totalIncome0.formatted("%.2f")),
      DisplayItem1("Capital Gain", (stats) => (stats.allTime.portfolioValues0.last._2 - stats.allTime.totalInvestment0 - stats.allTime.totalIncome0).formatted("%.2f")),
      DisplayItem1("Last instalment", (stats) => stats.allTime.instalments0.last._2.formatted("%.2f")),
      DisplayItem1("Last 12M Income", (stats) => stats.allTime.last12MonthsIncome.formatted("%.2f"))
    )

    trait View[T] {
      def view(something: T): String
    }
    implicit object ReturnView extends View[Double] {
      implicit def view(d: Double): String = f"${d * 100}%.1f%%"
    }
    implicit object AbsoluteDrawdownView extends View[AbsoluteDrawdown] {
      implicit def view(d: AbsoluteDrawdown): String = f"${d.amount}%.2f\n(${d.ratio * 100}%.1f%%)"
    }
    implicit object RelativeDrawdownView extends View[RelativeDrawdown] {
      implicit def view(d: RelativeDrawdown): String = f"${d.ratio * 100}%.1f%%\n(${d.amount}%.2f)"
    }

    class DisplayMeasure[+T](val name: String, private val source: (Statistics) => Option[Measure[BWML[T]]])(implicit v: View[T]) {
      private def createMeasureProperty(extract: Measure[BWML[T]] => String): ReadOnlyStringProperty = {
        val wrapper = ReadOnlyStringWrapper("")
        val property: ReadOnlyStringProperty = wrapper.readOnlyProperty
        wrapper <== Bindings.createStringBinding(
          () => source(SimulationModel.statistics.value) map extract getOrElse "N/A",
          SimulationModel.statistics)
        property
      }
      val adjustedBest: ReadOnlyStringProperty = createMeasureProperty((measure) => v.view(measure.adjusted.best))
      val adjustedWorst: ReadOnlyStringProperty = createMeasureProperty((measure) => v.view(measure.adjusted.worst))
      val adjustedMedian: ReadOnlyStringProperty = createMeasureProperty((measure) => v.view(measure.adjusted.median))
      val adjustedLast: ReadOnlyStringProperty = createMeasureProperty((measure) => v.view(measure.adjusted.last))
      val nominalBest: ReadOnlyStringProperty = createMeasureProperty((measure) => v.view(measure.nominal.best))
      val nominalWorst: ReadOnlyStringProperty = createMeasureProperty((measure) => v.view(measure.nominal.worst))
      val nominalMedian: ReadOnlyStringProperty = createMeasureProperty((measure) => v.view(measure.nominal.median))
      val nominalLast: ReadOnlyStringProperty = createMeasureProperty((measure) => v.view(measure.nominal.last))
    }

    private def measuresFor(stats: Statistics, period: Int): Option[Results] =
      if (stats == null) None else stats.byPeriod.get(period)

    val measures: Map[Int, IndexedSeq[DisplayMeasure[Any]]] =
      (for (period <- Statistics.periodsOfInterest) yield
        period ->
          IndexedSeq(
            new DisplayMeasure[Double]("Return", measuresFor(_, period) map (_.returnOnInvestment)),
            new DisplayMeasure[AbsoluteDrawdown]("Absolute Drawdown", measuresFor(_, period) map (_.absoluteDrawdown)),
            new DisplayMeasure[AbsoluteDrawdown]("Maximum Drawdown", measuresFor(_, period) map (_.maximumDrawdown)),
            new DisplayMeasure[RelativeDrawdown]("Relative Drawdown", measuresFor(_, period) map (_.relativeDrawdown))
          )).toMap
  }

  def intField(_id: String, bindTo: IntegerProperty): TextField =
    new TextField {
      id = _id
      alignment = javafx.geometry.Pos.CENTER_RIGHT
      textFormatter = new TextFormatter(
        javafx.scene.control.TextFormatter.IDENTITY_STRING_CONVERTER,
        bindTo.value.toString, { change => if (change.text.matches("[0-9]*")) change else null }) {}
      // Only update the underlying property when losing focus (this also avoids the infinite loop)
      // TODO: Also update when the users pauses typing for, say, 0.5s
      focused.onChange { (_, _, newVal) => if (!newVal) bindTo.value = text.value.toInt }
      bindTo.onChange { (_, _, newVal) => text = newVal.toString }
    }

  def controls: Node =
    new VBox {
      styleClass += "controls"
      children = List(
        new Button("Edit Allocation") {
          onAction = (e: ActionEvent) => {rootNode.children = alllocationEditor}
          maxWidth = Double.MaxValue
        },
        new Label("Initial Amount"),
        intField("initial-amount", SimulationModel.initialAmount),
        new Label("Instalments"),
        new ChoiceBox[InstalmentRuleID] {
          maxWidth = Double.MaxValue
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
                  val ii = intField("initial-instalment", SimulationModel.initialInstalment)
                  ii.disable <== SimulationModel.instalmentRuleId === SalaryPercentage
                  ii
                })
            }, new HBox {
              alignment = javafx.geometry.Pos.CENTER_LEFT
              spacing = 5
              visible <== SimulationModel.instalmentRuleId === SalaryPercentage
              children = List(
                {
                  val sp = intField("salary-percentage", SimulationModel.salaryPercentage)
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
          maxWidth = Double.MaxValue
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
    val maxPeriod = IntegerProperty(0)
    vgrow = Priority.Always
    tabs = Seq(
      new Tab {
        text = "Chart"
        closable = false
        content = lineChart
      },
      new Tab {
        text = "All Time"
        closable = false
        content =
            new GridPane {
              padding = Insets(15)
              hgap = 8
              vgap = 8
              columnConstraints = Seq(
                new ColumnConstraints { percentWidth = 50 },
                new ColumnConstraints { percentWidth = 50 }
              )
              for (i <- DisplayStats.items.indices;
                   item = DisplayStats.items(i)) {
                add(new Label {
                  text = item.name + ":"
                  maxWidth = Double.MaxValue
                  alignment = javafx.geometry.Pos.CENTER_RIGHT
                  style = "-fx-font-weight: bold; -fx-font-size: 150%"
                }, 0, i)
                item match {
                  case di1@DisplayItem1(_, _) =>
                    add(new Label {
                      text <== di1.property
                      maxWidth = Double.MaxValue
                      style = "-fx-font-size: 150%"
                    }, 1, i)
                  case di2@DisplayItem2(_, _, _) =>
                    add(new Label {
                      text <== di2.nominalValue
                      maxWidth = Double.MaxValue
                      style = "-fx-font-size: 150%"
                      visible <== !adjustForInflation
                    }, 1, i)
                    add(new Label {
                      text <== di2.deflatedValue
                      maxWidth = Double.MaxValue
                      style = "-fx-font-size: 150%"
                      visible <== adjustForInflation
                    }, 1, i)
                }
              }
            }
        }
    )
  }

  for (period <- Statistics.periodsOfInterest) {
    tabpane +=
      new Tab {
        text = s"${period}Y"
        closable = false
        disable <== tabpane.maxPeriod < period
        content = new VBox {
          children = Seq(
            new GridPane {
              padding = Insets(15)
              hgap = 8
              vgap = 8
              columnConstraints = Seq(
                new ColumnConstraints { percentWidth = 25 },
                new ColumnConstraints { percentWidth = 25 },
                new ColumnConstraints { percentWidth = 25 },
                new ColumnConstraints { percentWidth = 25 }
              )
              maxWidth = Double.MaxValue
              for ((measure, i) <- DisplayStats.measures(period) zipWithIndex) {
                add(new Label {
                  text = measure.name
                  maxWidth = Double.MaxValue
                  style = "-fx-font-weight: bold; -fx-font-size: 120%"
                  alignment = javafx.geometry.Pos.CENTER
                }, 0, i * 3, 4, 1)
                for ((title, column) <- Seq("Best", "Worst", "Median", "Last") zipWithIndex) {
                  add(new Label {
                    text = title
                    maxWidth = Double.MaxValue
                    alignment = javafx.geometry.Pos.CENTER
                    textAlignment = javafx.scene.text.TextAlignment.CENTER
                  }, column, i * 3 + 1)
                }
                for ((property, column) <- Seq(measure.nominalBest, measure.nominalWorst, measure.nominalMedian, measure.nominalLast).zipWithIndex) {
                  add(new Label {
                    text <== property
                    maxWidth = Double.MaxValue
                    style = "-fx-font-size: 120%"
                    alignment = javafx.geometry.Pos.CENTER
                    textAlignment = javafx.scene.text.TextAlignment.CENTER
                    visible <== !adjustForInflation
                  }, column, i * 3 + 2)
                }
                for ((property, column) <- Seq(measure.adjustedBest, measure.adjustedWorst, measure.adjustedMedian, measure.adjustedLast).zipWithIndex) {
                  add(new Label {
                    text <== property
                    maxWidth = Double.MaxValue
                    style = "-fx-font-size: 120%"
                    alignment = javafx.geometry.Pos.CENTER
                    textAlignment = javafx.scene.text.TextAlignment.CENTER
                    visible <== adjustForInflation
                  }, column, i * 3 + 2)
                }
              }
            }
          )
        }
      }
  }

  val chart = new BorderPane {
    left = controls
    center = new VBox {
      alignment = javafx.geometry.Pos.CENTER
      children = Seq(
        tabpane,
        new CheckBox {
          id = "deflate"
          text = "Adjust for inflation"
          selected <==> adjustForInflation
        }
      )
    }
  }

  val rootNode = new StackPane {
    children = chart
  }

  case class Letterbox(boxWidth: Double, boxHeight: Double) {
    def fit(width: Double, height: Double): (Double, Double) = {
      val scale = (boxWidth / width) min (boxHeight / height) min 1.0
      (width * scale, height * scale)
    }
  }

  val screen = Screen.primary

  val visualBoundsFactor =
    (screen.visualBounds.width / screen.bounds.width) min
    (screen.visualBounds.height / screen.bounds.height)
  val letterbox = Letterbox(screen.bounds.width * visualBoundsFactor, screen.bounds.height * visualBoundsFactor)
  val screenAspectRatio = screen.bounds.width / screen.bounds.height

  stage = new PrimaryStage {
    title = "Instrument Comparison"
    private val (initWidth, initHeight) = letterbox.fit(600 * screenAspectRatio, 600)
    scene = new Scene(initWidth, initHeight) {
      root = rootNode
      minWidth = initWidth
      minHeight = initHeight
      height.onChange {
        rootNode.style = f"-fx-font-size: ${12*stage.height.value/600}%.0fpx"
      }
      stylesheets.add("file:src/resources/custom.css")
    }
  }

  SimulationModel.statistics.onChange {
    updateChart
    tabpane.maxPeriod.value =
      if (SimulationModel.statistics == null) 0
      else SimulationModel.statistics.value.byPeriod.keys.max
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
  val dateRange =
    (settings \ "dateRange").headOption.collect { case node =>
      Storage.fromXML[DateRange](node) match {
        case Some(range) => println("Date Range loaded: " + range)
          AllocationModel.dateRange.value = range
        case None => println("Date range not loaded")
      }
    }

  AllocationModel.init(allocation.getOrElse(List.empty))
  val alllocationEditor = AllocationEditor.init(() => {
    SimulationModel.allocationProperty.value =
      if (AllocationModel.length > 0) new FixedAllocation(AllocationModel.get.toMap)
      else null
    SimulationModel.minStartDate.value =
      if (AllocationModel.length > 0)
        AllocationModel.availableInstruments map (_.startDate) reduce ((ym1, ym2) => if (ym1.isAfter(ym2)) ym1 else ym2)
      else Instrument.startDate
    rootNode.children = chart
  })

  def stopApp(): Unit = {
    val x = <settings>
      {Storage.toXML(AllocationModel.dateRange.value)}
      {if (AllocationModel.length > 0) Storage.toXML(AllocationModel.getWeights)}
      <initialAmount>{SimulationModel.initialAmount.value}</initialAmount>
      <initialInstalment>{SimulationModel.initialInstalment.value}</initialInstalment>
      {Storage.toXML(SimulationModel.instalmentRuleId.value)}
      {Storage.toXML(SimulationModel.strategyId.value)}
    </settings>
    xml.XML.save(".portfolio", x.head, "utf-8")
  }
}

