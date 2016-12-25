package investment.instruments

import java.time.YearMonth

import investment.data.{ExchangeRates, XAUUSD}

case object Gold extends Instrument {
  override def toString = "Gold"

  override val startDate = XAUUSD.startDate
  override val endDate = XAUUSD.endDate

  private val USDRUB = new ExchangeRates("USD")
  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = XAUUSD.mid(ym) * USDRUB.mid(ym)

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to the price */
  override def yieldPercentage(ym: YearMonth): Double = 0.0

}

