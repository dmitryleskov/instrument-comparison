package investment

import java.io.FileNotFoundException
import java.time.YearMonth

import scala.collection.mutable

case class Stock(ticker: String,
                 highs: Map[YearMonth, Double],
                 lows: Map[YearMonth, Double],
                 dividends: Map[YearMonth, Double],
                 splits: Map[YearMonth, Double] = Map()) extends Instrument {

  override def toString = ticker

  override lazy val startDate = highs.keys.min

  override def price(ym: YearMonth) = (highs(ym) + lows(ym)) / 2.0

  override def yieldPercentage(ym: YearMonth): Double = dividends.getOrElse(ym, 0.0) / price(ym)
}

object Stock {
  val dateFormat = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")

  def apply(ticker: String) = {
    val quotesFile = new CSVFile("data/" + ticker + ".csv")
    val highs = mutable.HashMap[YearMonth, Double]()
    val lows = mutable.HashMap[YearMonth, Double]()
    for (values <- quotesFile) {
      val ym = YearMonth.parse(values(2), dateFormat)
      highs(ym) = values(5).toDouble
      lows(ym) = values(6).toDouble
    }
    val dividendsFile = new CSVFile("data/" + ticker + "-dividends.csv")
    val dividends = mutable.HashMap[YearMonth, Double]()
    for (values <- dividendsFile) {
      val ym = YearMonth.parse(values(0), dateFormat)
      val amount = values(1).toDouble
      // Dividends for two periods may have the same ex-date, see e.g. MGNT 2009, 2011-12
      dividends(ym) = dividends.getOrElse(ym, 0.0) + amount
    }
    val splits = mutable.HashMap[YearMonth, Double]()
    try {
      for (values <- new CSVFile("data/" + ticker + "-splits.csv"))
        splits(YearMonth.parse(values(0), dateFormat)) = values(1).toDouble
    } catch {
      case fnf: FileNotFoundException => ()
    }
    new Stock(ticker, Map() ++ highs, Map() ++ lows, Map() ++ dividends, Map() ++ splits)
  }
}

