package investment

import java.time.{Year, YearMonth}

abstract class InstalmentRule(val start: YearMonth) {
  /** Returns amount invested in rubles during n-th month, numbered from 1 */
  def instalment(n: Int): Double
}

/** Invest a fixed amount each month */
class FixedAmount(override val start: YearMonth, val amount: Double) extends InstalmentRule(start) {
  override def instalment(n: Int): Double = amount
}

class FixedUSDAmount(override val start: YearMonth, val usdAmount: Double) extends InstalmentRule(start){
  override def instalment(n: Int): Double = usdAmount * USDRUB.mid(start.plusMonths(n-1))
}

/** Increase the invested amount by the given percentage each year */
class AnnualIncrease (override val start: YearMonth, val initialAmount: Double, annualIncrease: Double) extends InstalmentRule(start) {
  override def instalment(n: Int): Double =
    if (n <= 12) initialAmount
    else annualIncrease * instalment(n - 12)
}

class InflationAdjusted(override val start: YearMonth, val initialAmount: Double) extends InstalmentRule(start) {
  override def instalment(n: Int): Double =
    if (n <= 12) initialAmount
    else Inflation.annual(Year.from(start.plusMonths(n - 12 - 1))) * instalment(n - 12)
}