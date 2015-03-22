package investment

import java.time.YearMonth

class CashUSD extends Instrument {
  override def toString = "USD"

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = USDRUB.mid(ym)

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to the price */
  override def yieldPercentage(ym: YearMonth): Double = 0.0

}
