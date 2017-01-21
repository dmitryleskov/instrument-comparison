/*
 * Copyright (c) 2017 Dmitry Leskov. All rights reserved.
 */

package investment

import java.time.temporal.ChronoUnit.YEARS
import javafx.scene.chart.PieChart

import investment.AllocationModel.DateRange.{All, AtLeast, OldestOnly}
import investment.instruments.Instrument

import scala.collection.JavaConversions._
import scala.collection.mutable
import scalafx.Includes._
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.collections.{ObservableBuffer, ObservableSet}
import scalafx.event.ActionEvent
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout._

object AllocationModel {
  sealed abstract class DateRange
  object DateRange {
    case object OldestOnly extends DateRange {
      override def toString: String = "Oldest Only"
    }
    case class AtLeast(years: Int) extends DateRange {
      override def toString: String = s"$years+ Years"
    }
    case object All extends DateRange
    val values: Seq[DateRange] = OldestOnly :: (Statistics.periodsOfInterest.reverse map AtLeast) ++ Seq(All)
  }
  val dateRange: ObjectProperty[DateRange] = ObjectProperty(this, "filter", All)

  val availableInstruments: ObservableSet[Instrument] = ObservableSet(instruments.all)
  dateRange.onChange {
    val (add, remove) = instruments.all.partition { (instrument) =>
      dateRange.value match {
        case OldestOnly => instrument.startDate == instruments.minStartDate
        case AtLeast(years) => instrument.startDate.until(instruments.minEndDate.minusMonths(1), YEARS) >= years
        case All => true
      }
    }
    availableInstruments --= remove
    availableInstruments ++= add
    ()
  }

  sealed trait Change
  case class Insert(pos: Int, instrument: Instrument, newWeight: Int) extends Change
  case class Remove(pos: Int) extends Change
  case class ChangeInstrument(pos: Int, newInstrument: Instrument) extends Change
  case class ChangeWeight(pos: Int, newWeight: Int) extends Change

  var listeners: List[Change => Unit] = Nil
  def onChange(listener: Change => Unit): Unit = listeners ::= listener
  private def notify(change: Change) = for (l <- listeners) l(change)

  private val allocation: mutable.Buffer[(Instrument, Int)] = mutable.Buffer[(Instrument, Int)]()
  private def indexOf(instrument: Instrument): Option[Int] = {
    val index = allocation indexWhere { case (i, _) => i == instrument }
    if (index >= 0) Some(index) else None
  }

  def init(newAllocation: List[(Instrument, Int)]): Unit = {
    allocation map (_._1) foreach remove
    newAllocation foreach {case (i, w) => append(i, w)}
  }

  def get = (for (p <- allocation) yield (p._1, p._2.toDouble)).toList
  def getWeights = allocation.toList
  def getInstruments = (for (p <- allocation) yield p._1).toList

  def length = allocation.length

  def append(instrument: Instrument, weight: Int = 1): Unit =
    indexOf(instrument) match {
      case Some(_) => ()
      case None =>
        allocation.append((instrument, weight))
        notify(Insert(allocation.length - 1, instrument, weight))
    }

  def insert(pos: Int, instrument: Instrument, weight: Int = 1): Unit = ???
//    indexOf(instrument) match {
//      case Some(_) => ()
//      case None => allocation :+= (instrument, weight)
//    }

  def update(instrument: Instrument, newWeight: Int): Unit =
    indexOf(instrument) match {
      case Some(index) =>
        allocation.update(index, (instrument, newWeight))
        notify(ChangeWeight(index, newWeight))
      case None => ()
    }

  def changeInstrument(oldInstrument: Instrument, newInstrument: Instrument): Unit =
    indexOf(newInstrument) match {
      case Some(_) => ()
      case None =>
        indexOf(oldInstrument) match {
          case Some(index) =>
            allocation.update(index, (newInstrument, allocation(index)._2))
            notify(ChangeInstrument(index, newInstrument))
          case None => ()
        }
    }

  def remove(instrument: Instrument): Unit =
    indexOf(instrument) match {
      case Some(index) =>
        allocation.remove(index)
        notify(Remove(index))
      case None => ()
    }
}

object AllocationEditor {
  import AllocationModel._

  def init(done: () => Unit): Node = {

    val chartData = ObservableBuffer[PieChart.Data]()

    AllocationModel.onChange {
      case Insert(pos, instrument, weight) =>
        chartData.insert(pos, new PieChart.Data(instrument.toString, weight))
      case ChangeInstrument(pos, newInstrument) =>
        chartData.insert(pos, new PieChart.Data(newInstrument.toString, chartData(pos).pieValue()))
        chartData.remove(pos + 1)
      case ChangeWeight(pos, newWeight) =>
        chartData(pos).pieValue = newWeight
      case Remove(pos) =>
        chartData.remove(pos)
    }

    class AllocationItemSelector {
      def this (_instrument: Instrument, _weight: Int) = {
        this
        instrument.value = _instrument
        weight.valueFactory.value.value = _weight
      }
      val instrument = new ChoiceBox[Instrument] {
        maxWidth = Double.MaxValue
        items = ObservableBuffer(instruments.all)

        // This is a workaround for the following problem:
        // When the very first ChoiceBox gets focus for the first time
        // and has its list of items changed in refreshMyItems() (the items.value.remove(0, drop) line),
        // it gets resized to minimal width as if there were no items,
        // and even loses focus if the mouse ends up being outside it
        // In fact, removing a single item seems to cause such effect.
        width.onChange ((_, oldValue, newValue) => if (oldValue.doubleValue <= 0.0) minWidth = newValue.doubleValue)

        def refreshMyItems() = {
          val selected = AllocationModel.getInstruments.toSet
          val selectable = instruments.all.filter(x => (availableInstruments contains x) && !(selected contains x))
          val currentIndex = selectionModel().selectedIndex.value
          if (currentIndex >= 0) {
            items.value.remove(0, currentIndex)
            items.value.remove(1, items.value.size)
            items.value.addAll(selectable)
          } else {
            val drop = items.value.size
            items.value.addAll(selectable)
            items.value.remove(0, drop)
          }
          ()
        }

        selectionModel().selectedItem.onChange((_, oldValue, newValue) => {
          if (oldValue == null) {
            AllocationModel.append(newValue)
            weight.disable = false
          } else {
            AllocationModel.changeInstrument(oldValue, newValue)
          }
          refreshMyItems()
        })
        focused.onChange { if (focused.value) refreshMyItems() }
      }
      // Spinner requires Java 8u40!!!
      val weight: Spinner[Int] = new Spinner[Int](1, 10, 1) {
        styleClass += Spinner.StyleClassSplitArrowsHorizontal
        styleClass += "weight"
        disable = true
        value.onChange { (_, _, newWeight) => AllocationModel.update(instrument.value(), newWeight) }
      }

      val delete = new Button("Delete") {
        style = "-fx-color: red"
        onAction = { (e: ActionEvent) => AllocationModel.remove(instrument.value.value) }
        visible = false
      }
    }

    val selectors = mutable.Buffer[AllocationItemSelector]()

    def addSelectorCommon(grid: GridPane, selector: AllocationItemSelector, pos: Int): Unit = {
      grid.add(selector.instrument, 0, pos)
      grid.add(selector.weight, 1, pos)
      grid.add(selector.delete, 1, pos)
      selectors += selector
    }

    def addEmptySelector(grid: GridPane): Unit =
      addSelectorCommon(grid, new AllocationItemSelector, AllocationModel.length)

    def addSelector(grid: GridPane, pos: Int, instrument: Instrument, weight: Int): Unit =
      addSelectorCommon(grid, new AllocationItemSelector(instrument, weight), pos)

    def removeSelector(grid: GridPane, pos: Int): Unit = {
      val selector = selectors(pos)
      selectors.remove(pos)
      val children = grid.getChildren
      children.removeAll(selector.instrument, selector.weight, selector.delete)

      for(child <- children) {
        val row = GridPane.getRowIndex(child)
        if (row > pos) GridPane.setRowIndex(child, row - 1)
      }
    }

    val deleteMode = BooleanProperty(false)

    deleteMode.onChange {
      val delete = deleteMode.value
      for(selector <- selectors) {
        val empty = selector.instrument.value.value == null
        selector.instrument.disable = delete
        selector.weight.visible = !delete || empty
        selector.delete.visible = delete && !empty
      }
    }

    val grid = new GridPane() { styleClass += "control-grid" }

    availableInstruments.onChange((set, change) => change match {
      case ObservableSet.Remove(instrument) => AllocationModel.remove(instrument)
      case _ => ()
    })

    val border = new BorderPane {
      left = new VBox {
        styleClass += "controls"
        hgrow = Priority.Never
        val doneButton = new Button("Done") {
          style = "-fx-font-weight: bold"
          onAction = (e: ActionEvent) => {
            if (deleteMode.value)
              deleteMode.value = false
            else
              done()
          }
        }

        val filterBox = new ChoiceBox[DateRange] {
          items = ObservableBuffer(DateRange.values)
          value <==> dateRange
        }
        val deleteButton: Button = new Button("Delete") {
          style = "-fx-color: red"
          onAction = (e: ActionEvent) => {
            deleteMode.value = !deleteMode.value
          }
        }

        deleteMode.onChange({deleteButton.visible = !deleteMode.value})

        AllocationModel.onChange {
          case Insert(pos, instrument, weight) =>
            // Assume append
            addEmptySelector(grid)
          // remove instrument from all boxes
          case ChangeInstrument(pos, newInstrument) => ()
          case ChangeWeight(pos, newWeight) => ()
          case Remove(pos) => removeSelector(grid, pos)
        }
        children = Seq(
          new HBox { children = Seq(doneButton, new Region {hgrow = Priority.Always}, filterBox) },
          grid,
          deleteButton)
      }
      center = new PieChart(chartData) {
        hgrow = Priority.Always
        maxWidth(Double.MaxValue)
      }
    }

    AllocationModel.getWeights.zipWithIndex.foreach {
      case ((i, w), p) =>
        chartData.insert(p, new PieChart.Data(i.toString, w))
        addSelector(grid, p, i, w)
    }
    addEmptySelector(grid)

    border
  }
}
