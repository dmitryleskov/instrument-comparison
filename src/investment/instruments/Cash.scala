package investment.instruments

import java.time.YearMonth

import investment.data.{EURRUB, USDRUB}

case object CashRUB extends Instrument {
  override def toString = "RUB"

  override val startDate = USDRUB.startDate
  override val endDate = USDRUB.endDate

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = 1.0

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to the price */
  override def yieldPercentage(ym: YearMonth): Double = 0.0
}

case object CashUSD extends Instrument {
  override def toString = "USD"

  override val startDate = USDRUB.startDate
  override val endDate = USDRUB.endDate

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = USDRUB.mid(ym)

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to the price */
  override def yieldPercentage(ym: YearMonth): Double = 0.0
}

case object CashEUR extends Instrument {
  override def toString = "EUR"

  override val startDate = EURRUB.startDate
  override val endDate = EURRUB.endDate

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = EURRUB.mid(ym)

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to the price */
  override def yieldPercentage(ym: YearMonth): Double = 0.0
}
