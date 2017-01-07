package investment.instruments

import java.io.FileNotFoundException
import java.time.Month.JANUARY
import java.time.YearMonth

import investment.util.CSVFile

import scala.collection.mutable

case class Stock(ticker: String) extends Instrument {
  private val dateFormat = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")

  private val splits: Map[YearMonth, Double] = {
    val splits = mutable.HashMap[YearMonth, Double]()
    try {
      for (values <- new CSVFile("data/" + ticker + "-splits.csv"))
        splits(YearMonth.parse(values(0), dateFormat)) = values(1).toDouble
    } catch {
      case fnf: FileNotFoundException => ()
    }
    Map() ++ splits
  }

  private val (open: Map[YearMonth, Double],
               high: Map[YearMonth, Double],
               low: Map[YearMonth, Double],
               close: Map[YearMonth, Double]
               ) = {
    val quotesFile = new CSVFile("data/" + ticker + ".csv")
    val open = mutable.HashMap[YearMonth, Double]()
    val high = mutable.HashMap[YearMonth, Double]()
    val low = mutable.HashMap[YearMonth, Double]()
    val close = mutable.HashMap[YearMonth, Double]()
    for (values <- quotesFile) {
      val ym = YearMonth.parse(values(2), dateFormat)
      val splitFactor = splits
        .filter { case (splitYM, _) => splitYM.isAfter(ym) }
        .foldLeft (1.0) { case (acc, (_, splitFactor)) => acc / splitFactor }
      open(ym) = values(4).toDouble * splitFactor
      high(ym) = values(5).toDouble * splitFactor
      low(ym) = values(6).toDouble * splitFactor
      close(ym) = values(7).toDouble * splitFactor
      assert(high(ym) >= low(ym))
      assert(high(ym) >= open(ym))
      assert(high(ym) >= close(ym))
      assert(low(ym) <= open(ym))
      assert(low(ym) <= close(ym))
    }
    (Map() ++ open, Map() ++ high, Map() ++ low, Map() ++ close)
  }

  /** Dividends already account for splits */
  private val dividends: Map[YearMonth, Double] = {
    def dividendTaxRate(ym: YearMonth) = if (ym.isBefore(YearMonth.of(2015, JANUARY))) 0.09 else 0.13
    val dividendsFile = new CSVFile("data/" + ticker + "-dividends.csv")
    val dividends = mutable.HashMap[YearMonth, Double]()
    for (values <- dividendsFile) {
      val ym = YearMonth.parse(values(0), dateFormat)
      val amount = values(1).toDouble * (1.0 - dividendTaxRate(ym))
      // Dividends for two periods may have the same ex-date, see e.g. MGNT 2009, 2011-12
      dividends(ym) = dividends.getOrElse(ym, 0.0) + amount
    }
    Map() ++ dividends
  }


  override def toString = ticker

  override lazy val startDate = high.keys.min
  override lazy val endDate = high.keys.max

  override def price(ym: YearMonth) = (high(ym) + low(ym)) / 2.0

  override def yieldPercentage(ym: YearMonth): Double = dividends.getOrElse(ym, 0.0) / price(ym)
}
