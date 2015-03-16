package investment

import java.time.YearMonth

case class Deposit(annualInterest: Double) extends Instrument {
  override def toString = "Deposit %.2f%%".format(annualInterest * 100)

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = 1.0

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to its value */
  override def yieldPercentage(ym: YearMonth): Double = annualInterest / 12
}

case class DepositUSD(annualInterest: Double) extends Instrument {
  override def toString = "DepositUSD %.2f%%".format(annualInterest * 100)

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = USDRUB.mid(ym)

  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to its value */
  override def yieldPercentage(ym: YearMonth): Double = annualInterest / 12
}
