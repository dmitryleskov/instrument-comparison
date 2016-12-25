package investment.instruments

import java.time.Month.JANUARY
import java.time.YearMonth

import investment.data.{EURRUB, ExchangeRates}

case object CashRUB extends Instrument {
  override def toString = "Cash RUB"

  override val startDate = YearMonth.of(1998, JANUARY)
  override val endDate = YearMonth.now

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = 1.0

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to the price */
  override def yieldPercentage(ym: YearMonth): Double = 0.0
}

case class Cash(currency: String) extends Instrument {
  override val toString = s"Cash $currency"

  private val rates = new ExchangeRates(currency)

  override val startDate = rates.startDate
  override val endDate = rates.endDate

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = rates.mid(ym)

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to the price */
  override def yieldPercentage(ym: YearMonth): Double = 0.0
}
