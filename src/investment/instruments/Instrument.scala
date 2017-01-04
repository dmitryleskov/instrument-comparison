package investment.instruments

import java.time.Month.JANUARY
import java.time.YearMonth

abstract class Instrument {
  def startDate: YearMonth = Instrument.startDate
  def endDate: YearMonth

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  def price(ym: YearMonth): Double

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to the price */
  def yieldPercentage(ym: YearMonth): Double
}
object Instrument {
  /** The minimum possble instrument history start date is right after ruble denomination */
  val startDate: YearMonth = YearMonth.of(1998, JANUARY)
}
