package investment

import javafx.scene.chart.PieChart

import scala.collection.JavaConversions._
import scala.collection.mutable
import scalafx.Includes._
import scalafx.beans.property.BooleanProperty
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Node
import scalafx.scene.control.{Button, ChoiceBox, Spinner}
import scalafx.scene.layout.{BorderPane, GridPane, VBox}

object PortfolioModel {
  sealed trait Change
  case class Insert(pos: Int, instrument: Instrument, newWeight: Int) extends Change
  case class Remove(pos: Int) extends Change
  case class ChangeInstrument(pos: Int, newInstrument: Instrument) extends Change
  case class ChangeWeight(pos: Int, newWeight: Int) extends Change
}

class PortfolioModel(init: List[(Instrument, Int)]) {
  import PortfolioModel._
  private val portfolio = init.to[mutable.Buffer]
  var listeners: List[Change => Unit] = Nil
  def onChange(listener: Change => Unit) = listeners ::= listener
  private def notify(change: Change) = for (l <- listeners) l(change)

  private def indexOf(instrument: Instrument): Option[Int] = {
    val index = portfolio indexWhere {case (i, _) => i == instrument}
    if (index >= 0) Some(index) else None
  }

  def get = (for (p <- portfolio) yield (p._1, p._2.toDouble)).toList
  def getWeights = portfolio.toList
  def getInstruments = (for (p <- portfolio) yield p._1).toList

  def length = portfolio.length

  def append(instrument: Instrument, weight: Int = 1) =
    indexOf(instrument) match {
      case Some(_) => ()
      case None =>
        portfolio.append((instrument, weight))
        notify(Insert(portfolio.length - 1, instrument, weight))
    }
  def insert(pos: Int, instrument: Instrument, weight: Int = 1) = ???
//    indexOf(instrument) match {
//      case Some(_) => ()
//      case None => portfolio :+= (instrument, weight)
//    }
  def update(instrument: Instrument, newWeight: Int) =
    indexOf(instrument) match {
      case Some(index) =>
        portfolio.update(index, (instrument, newWeight))
        notify(ChangeWeight(index, newWeight))
      case None => ()
    }

  def changeInstrument(oldInstrument: Instrument, newInstrument: Instrument) =
    indexOf(newInstrument) match {
      case Some(_) => ()
      case None =>
        indexOf(oldInstrument) match {
          case Some(index) =>
            portfolio.update(index, (newInstrument, portfolio(index)._2))
            notify(ChangeInstrument(index, newInstrument))
          case None => ()
        }
    }

  def remove(instrument: Instrument) =
    indexOf(instrument) match {
      case Some(index) =>
        portfolio.remove(index)
        notify(Remove(index))
      case None => ()
    }

}

object PortfolioEditor {
  import PortfolioModel._

  def init(model: PortfolioModel, done: () => Unit): Node = {

    val chartData = ObservableBuffer[PieChart.Data]()

    class PortfolioItemSelector {
      def this (_instrument: Instrument, _weight: Int) = {
        this
        println("Adding selector: " + _instrument + " " + _weight)
        instrument.value = _instrument
        weight.valueFactory.value.value = _weight
      }
      val instrument = new ChoiceBox[Instrument] {
        items = ObservableBuffer(Instruments.all)
        margin = Insets(5, 5, 5, 0)

        // This is a workaround for the following problem:
        // When the very first ChoiceBox's list of items gets changed in focused.onChange()
        // that ChoiceBox gets resized to minimal width as if there were no items,
        // and even loses focus if the mouse ends up being outside it
        width.onChange({if (width.value > minWidth.value) minWidth = width.value})

        def refreshMyItems() = {
          val selected = model.getInstruments.toSet
          val available = Instruments.all.filter(x => !(selected contains x))
          println(available)
          val currentIndex = selectionModel().selectedIndex.value
          if (currentIndex >= 0) {
            items.value.remove(0, currentIndex)
            items.value.remove(1, items.value.size)
            items.value.addAll(available)
          } else {
            val drop = items.value.size
            items.value.addAll(available)
            items.value.remove(0, drop)
          }
          println(items)
          ()
        }

        selectionModel().selectedItem.onChange((_, oldValue, newValue) => {
          if (oldValue == null) {
            model.append(newValue)
            weight.disable = false
          } else {
            model.changeInstrument(oldValue, newValue)
          }
          refreshMyItems()
        })
        focused.onChange({
          if (focused.value) {
            println("Focus!")
            refreshMyItems()
          }
        })
      }
      // Spinner requires Java 8u40!!!
      val weight: Spinner[Int] = new Spinner[Int](1, 10, 1) {
        maxWidth = 75
        styleClass += Spinner.StyleClassSplitArrowsHorizontal
        disable = true
        value.onChange((_, _, newWeight) => {
          println(instrument.value())
          model.update(instrument.value(), newWeight)
        })
      }

      val delete = new Button("Delete") {
        style = "-fx-color: red"
        onAction = (e: ActionEvent) => {
          model.remove(instrument.value.value)
        }
        visible = false
      }
    }

    val selectors = mutable.Buffer[PortfolioItemSelector]()

    def addEmptySelector(grid: GridPane): Unit = {
      println(model.length)
      val selector = new PortfolioItemSelector
      grid.add(selector.instrument, 0, model.length + 1)
      grid.add(selector.weight, 1, model.length + 1)
      grid.add(selector.delete, 1, model.length + 1)
      selectors += selector
    }

    def addSelector(grid: GridPane, pos: Int, _instrument: Instrument, _weight: Int): Unit = {
      val selector = new PortfolioItemSelector(_instrument, _weight)
      grid.add(selector.instrument, 0, pos)
      grid.add(selector.weight, 1, pos)
      grid.add(selector.delete, 1, pos)
      selectors += selector
    }

    def removeSelector(grid: GridPane, pos: Int): Unit = {
      println("Remove", pos)
      val selector = selectors(pos)
      println(selector)
      selectors.remove(pos)
      val children = grid.getChildren
      children.removeAll(selector.instrument, selector.weight, selector.delete)

      for(child <- children) {
        val row = GridPane.getRowIndex(child)
        if (row > pos) GridPane.setRowIndex(child, row - 1)
      }
    }

    val deleteMode = BooleanProperty(false)

    deleteMode.onChange({
      val delete = deleteMode.value
      for(selector <- selectors) {
        val empty = selector.instrument.value.value == null
        selector.instrument.disable = delete
        selector.weight.visible = !delete || empty
        selector.delete.visible = delete && !empty
      }
    })

    model.onChange((change) => {
      println("model.onChange(view)")
      change match {
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
    })

    val grid = new GridPane() {
      grid =>
      padding = Insets(15, 0, 15, 0)
      style = "-fx-border-width: 1 0; -fx-border-color: grey"
    }
    val border = new BorderPane {
      left = new VBox {
        val doneButton = new Button("Done") {
          onAction = (e: ActionEvent) => {
            if (deleteMode.value)
              deleteMode.value = false
            else
              done()
          }
        }
        val cancelButton = new Button("Cancel") {
          onAction = (e: ActionEvent) => {}
        }
        val deleteButton: Button = new Button("Delete") {
          style = "-fx-color: red"
          onAction = (e: ActionEvent) => {
            deleteMode.value = !deleteMode.value
          }
        }
        deleteMode.onChange({deleteButton.visible = !deleteMode.value})

        model.onChange(change => {
          println("model.onChange(controller)")
          change match {
            case Insert(pos, instrument, weight) =>
              println("Insert")
              // Assume append
              addEmptySelector(grid)
            // remove instrument from all boxes
            case ChangeInstrument(pos, newInstrument) => ()
            case ChangeWeight(pos, newWeight) => ()
            case Remove(pos) =>
              println("Remove")
              removeSelector(grid, pos)
          }
        })
        children = List(doneButton, cancelButton, grid, deleteButton)
        spacing = 10
        padding = Insets(5)
        alignment = Pos.TopCenter
      }
      center = new PieChart(chartData)
    }

    model.getWeights.zipWithIndex.foreach {
      case ((i, w), p) =>
        chartData.insert(p, new PieChart.Data(i.toString, w))
        addSelector(grid, p, i, w)
    }
    addEmptySelector(grid)

    border
  }
}
