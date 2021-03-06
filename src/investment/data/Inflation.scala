package investment.data

import java.time.YearMonth
import java.util.Locale
import investment.instruments.Instrument
import investment.util.CSVFile

import scala.collection.mutable

case object Inflation extends Instrument {
  override def toString = "Inflation"

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = 1.0

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to its value */
  override def yieldPercentage(ym: YearMonth): Double = rates(ym)

  override lazy val startDate = rates.keys.min
  override lazy val endDate = rates.keys.max

  val rates: Map[YearMonth, Double] = {
    val dateFormat = java.time.format.DateTimeFormatter.ofPattern("MMM-yyyy", Locale.US)
    val csv = new CSVFile("data/Inflation.csv")
    val data = mutable.HashMap[YearMonth, Double]()
    for (values <- csv) {
      val yearmonth = YearMonth.parse(values(0), dateFormat)
      val rate = values(1).toDouble / 100
      data(yearmonth) = rate
    }
    Map() ++ data
  }
  def aggregate(from: YearMonth, to: YearMonth): Double = ???
}
