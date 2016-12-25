package investment.instruments

import java.time.Month.JANUARY
import java.time.YearMonth

abstract class Instrument {
  /** The minimum possble instrument history start date is right after denomination */
  def startDate = YearMonth.of(1998, JANUARY)
  def endDate: YearMonth

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  def price(ym: YearMonth): Double

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to the price */
  def yieldPercentage(ym: YearMonth): Double
}
