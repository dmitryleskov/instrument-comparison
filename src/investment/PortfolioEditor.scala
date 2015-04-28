package investment

import javafx.scene.chart.PieChart

import scala.collection.mutable
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableMap.Replace
import scalafx.collections.{ObservableMap, ObservableBuffer}
import scalafx.event.ActionEvent
import scalafx.geometry.{Pos, HPos, Insets}
import scalafx.scene.Scene
import scalafx.scene.control.{Spinner, ChoiceBox, Button}
import scalafx.scene.layout.{VBox, GridPane, BorderPane}


object PortfolioModel {
  sealed trait Change
  case class Insert(pos: Int, instrument: Instrument, newWeight: Int) extends Change
  case class Remove(pos: Int) extends Change
  case class ChangeInstrument(pos: Int, newInstrument: Instrument) extends Change
  case class ChangeWeight(pos: Int, newWeight: Int) extends Change
}

class PortfolioModel {
  import PortfolioModel._
  private val portfolio = mutable.Buffer[(Instrument, Int)]()
  var listeners: List[Change => Unit] = Nil
  def onChange(listener: Change => Unit) = listeners ::= listener
  private def notify(change: Change) = for (l <- listeners) l(change)

  private def indexOf(instrument: Instrument): Option[Int] = {
    val index = portfolio indexWhere {case (i, _) => i == instrument}
    if (index >= 0) Some(index) else None
  }
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
    indexOf(oldInstrument) match {
      case Some(index) =>
        portfolio.update(index, (newInstrument, portfolio(index)._2))
        notify(ChangeInstrument(index, newInstrument))
      case None => ()
    }
  def remove(instrument: Instrument) =
    indexOf(instrument) match {
      case Some(index) =>
        portfolio.remove(index)
        notify(Remove(index))
      case None => ()
    }
}

object PortfolioEditor extends JFXApp {
  import PortfolioModel._
  val model = new PortfolioModel

  val chartData = ObservableBuffer[PieChart.Data]()

  class PortfolioItemSelector (val onFirstSelect: () => Unit) {
    val instrument = new ChoiceBox[Instrument] {
      maxWidth = 200
      items = ObservableBuffer(Instruments.all)
      selectionModel().selectedItem.onChange((_, oldValue, newValue) => {
        if (oldValue == null) {
          model.append(newValue)
          weight.disable = false
          onFirstSelect()
        } else {
          model.changeInstrument(oldValue, newValue)
        }
      })
    }
    // Spinner requires Java 8u40!!!
    val weight: Spinner[Int] = new Spinner[Int](1, 10, 1) {
      maxWidth = 100
      styleClass += Spinner.StyleClassSplitArrowsHorizontal
      disable = true
      value.onChange((_ , _,newWeight) => {
        println(instrument.value())
        model.update(instrument.value(), newWeight)
      })
    }
  }

  def addEmptySelector(grid: GridPane): Unit = {
    val selector = new PortfolioItemSelector(() => {println("bla");addEmptySelector(grid)})
    grid.add(selector.instrument, 0, chartData.length + 1)
    grid.add(selector.weight, 1, chartData.length + 1)
  }

  model.onChange((change) => {
    println("model.onChange called")
    println(change)
    change match {
      case Insert(pos, instrument, weight) =>
        chartData.insert(pos, new PieChart.Data(instrument.toString, weight))
      case ChangeInstrument(pos, newInstrument) =>
        chartData.insert(pos, new PieChart.Data(newInstrument.toString, chartData(pos).pieValue()))
        chartData.remove(pos + 1)
      case ChangeWeight(pos, newWeight) =>
        chartData(pos).pieValue = newWeight
    }
  })

  val border = new BorderPane {
    left = new VBox {
      val saveButton = new Button ("Save & Close")
      saveButton.onAction = (e: ActionEvent) => {
      }
      val cancelButton = new Button ("Cancel")
      cancelButton.onAction = (e: ActionEvent) => {
      }
      val grid = new GridPane() {
        grid =>
        padding = Insets(15, 0, 15,0 )
        addEmptySelector(grid)
      }
      children = List(saveButton, cancelButton, grid)
      spacing = 10
      padding = Insets(5)
      alignment = Pos.TopCenter
    }
    center = new PieChart(chartData)
  }

  stage = new PrimaryStage {
    title = "Portfolio Editor"
    scene = new Scene(800, 600) {
      root = border
    }

  }
}
