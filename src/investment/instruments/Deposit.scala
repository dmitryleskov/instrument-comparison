package investment.instruments

import java.time.YearMonth
import java.util.Locale
import investment.data.ExchangeRates
import investment.util.CSVFile

import scala.collection.mutable

case object DepositRUB extends Instrument {
  override def toString = "Deposit RUB"

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = 1.0

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to its value */
  override def yieldPercentage(ym: YearMonth): Double = math.pow(1.0 + interest(ym), 1.0 / 12.0) - 1.0

  override lazy val startDate = interest.keys.min
  override lazy val endDate = interest.keys.max

  private val interest: Map[YearMonth, Double] = {
    val dateFormat = java.time.format.DateTimeFormatter.ofPattern("MMM-yy", Locale.US)
    val csv = new CSVFile("data/Deposit-RUB.csv")
    val data = mutable.HashMap[YearMonth, Double]()
    for (values <- csv) {
      val yearmonth = YearMonth.parse(values(0), dateFormat)
      val rate = values(1).toDouble / 100
      data(yearmonth) = rate
    }
    Map() ++ data
  }
}

case class Deposit(currency: String, annualInterest: Double) extends Instrument {
  override val toString = f"Deposit $currency%s ${annualInterest * 100}%.2f%%"

  private val rates = new ExchangeRates(currency)

  override lazy val startDate = rates.startDate.plusMonths(1)
  override lazy val endDate = rates.endDate

  /** The price in rubles of purchasing one unit of the given instrument in the given month.
    *
    * @param ym
    * @return the midpoint CBR rate for the month
    */
  override def price(ym: YearMonth): Double = rates.mid(ym)

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to its value */
  override def yieldPercentage(ym: YearMonth): Double = math.pow(1.0 + annualInterest, 1.0 / 12.0) - 1.0
}
