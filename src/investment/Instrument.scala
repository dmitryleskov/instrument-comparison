package investment

import java.time.YearMonth

abstract class Instrument {
  /** The minimum possble instrumennt history start date is right after denomination */
  def startDate = YearMonth.parse("1998-01")

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  def price(ym: YearMonth): Double

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to the price */
  def yieldPercentage(ym: YearMonth): Double
}
