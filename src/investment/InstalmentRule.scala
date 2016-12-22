package investment

import java.time.{Year, YearMonth}

import investment.data.{AverageSalary, Inflation, USDRUB}

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

/** Increase the invested amount by official inflation each month */
class InflationAdjusted(override val start: YearMonth, val initialAmount: Double) extends InstalmentRule(start) {
  override def instalment(n: Int): Double =
    if (n == 1) initialAmount
    else (1 + Inflation.rates(start.plusMonths(n - 1))) * instalment(n - 1)
}

/** Percentage of the official average salary */
class SalaryPercentage(override val start: YearMonth, val percentage: Double) extends InstalmentRule(start) {
  override def instalment(n: Int): Double =
    AverageSalary.rates(start.plusMonths(n - 1)) * percentage
}