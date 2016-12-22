package investment.instruments

import java.io.FileNotFoundException
import java.time.YearMonth

import investment.util.CSVFile

import scala.collection.mutable

case class Stock(ticker: String,
                 open: Map[YearMonth, Double],
                 high: Map[YearMonth, Double],
                 low: Map[YearMonth, Double],
                 close: Map[YearMonth, Double],
                 dividends: Map[YearMonth, Double],
                 splits: Map[YearMonth, Double] = Map()) extends Instrument {

  override def toString = ticker

  override lazy val startDate = high.keys.min
  override lazy val endDate = high.keys.max

  override def price(ym: YearMonth) = (high(ym) + low(ym)) / 2.0

  override def yieldPercentage(ym: YearMonth): Double = dividends.getOrElse(ym, 0.0) / price(ym)
}

object Stock {
  val dateFormat = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")

  def apply(ticker: String) = {
    val quotesFile = new CSVFile("data/" + ticker + ".csv")
    val open = mutable.HashMap[YearMonth, Double]()
    val high = mutable.HashMap[YearMonth, Double]()
    val low = mutable.HashMap[YearMonth, Double]()
    val close = mutable.HashMap[YearMonth, Double]()
    for (values <- quotesFile) {
      val ym = YearMonth.parse(values(2), dateFormat)
      open(ym) = values(4).toDouble
      high(ym) = values(5).toDouble
      low(ym) = values(6).toDouble
      close(ym) = values(7).toDouble
      assert(high(ym) >= low(ym))
      assert(high(ym) >= open(ym))
      assert(high(ym) >= close(ym))
      assert(low(ym) <= open(ym))
      assert(low(ym) <= close(ym))
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
    new Stock(ticker, Map() ++ open, Map() ++ high, Map() ++ low, Map() ++ close, Map() ++ dividends, Map() ++ splits)
  }
}