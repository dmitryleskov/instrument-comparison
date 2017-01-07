package investment

import java.time.{Year, YearMonth}

import investment.data.{AverageSalary, Inflation, ExchangeRates}

abstract class InstalmentRule() {
  /** Returns amount invested in rubles during n-th month from {@code start}, numbered from 1 */
  def instalment(start: YearMonth, n: Int): Double
}

/** Invest a fixed amount each month */
class FixedAmount(val amount: Double) extends InstalmentRule {
  override def instalment(start: YearMonth, n: Int): Double = amount
}

//class FixedUSDAmount(override val start: YearMonth, val usdAmount: Double) extends InstalmentRule(start){
//  override def instalment(n: Int): Double = usdAmount * Currency.mid(start.plusMonths(n-1))
//}

/** Increase the invested amount by the given percentage each year */
class AnnualIncrease (val initialAmount: Double, annualIncrease: Double) extends InstalmentRule {
  override def instalment(start: YearMonth, n: Int): Double =
    if (n <= 12) initialAmount
    else annualIncrease * instalment(start, n - 12)
}

/** Increase the invested amount by official inflation each month */
class InflationAdjusted(val initialAmount: Double) extends InstalmentRule {
  override def instalment(start: YearMonth, n: Int): Double =
    if (n == 1) initialAmount
    else (1 + Inflation.rates(start.plusMonths(n - 1))) * instalment(start, n - 1)
}

/** Percentage of the official average salary */
class SalaryPercentage(val percentage: Double) extends InstalmentRule {
  override def instalment(start: YearMonth, n: Int): Double =
    AverageSalary.rates(start.plusMonths(n - 1)) * percentage
}