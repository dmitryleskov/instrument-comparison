package investment

import java.time.YearMonth
import java.time.temporal.ChronoUnit

import investment.data.{AverageSalary, Inflation}

class Statistics(val simulator: Simulator) {
  sealed case class Drawdown(date: YearMonth, amount: Double, ratio: Double) {
    def maxByAmount(other: Drawdown) = if (other.amount > amount) other else this
    def maxByRatio(other: Drawdown) = if (other.ratio > ratio) other else this
    override def toString = if (amount > 0) f"$amount%.2f ${ratio * 100}%.1f%% $date" else "0.0%"
  }
  case object Drawdown {
    val zero = Drawdown(start, 0.0, 0.0)
  }

  /** @return Maximum difference between investment-to-date and current portfolio value */
  private def absoluteDrawdown(valuations: List[(YearMonth, Double)], baseline: List[(YearMonth, Double)]) =
    ((valuations zip baseline) map {
      case ((ymv, v), (ymb, b)) if ymv == ymb => Drawdown(ymv, b - v, (b - v) / b)
    }).foldLeft(Drawdown.zero)(_ maxByAmount _)

  private def drawdown(valuations: List[(YearMonth, Double)], max: (Drawdown, Drawdown) => Drawdown) =
    (valuations drop 1).foldLeft(valuations.head._2, valuations.head._2, Drawdown.zero) {
      case ((lastMax, lastMin, res), (ym, v)) =>
        if (v > lastMax)
          (v, v, max(res, Drawdown(ym, lastMax - lastMin, (lastMax - lastMin) / lastMax)))
        else
          (lastMax, lastMin min v, res)
    }._3

  /** @return Maximum difference between a peak and the subsequent trough */
  private def maximumDrawdown(valuations: List[(YearMonth, Double)]) =
    drawdown(valuations, _ maxByAmount _)

  /** @return Maximum ratio of the difference between a peak and the subsequent trough over the peak */
  private def relativeDrawdown(valuations: List[(YearMonth, Double)]) =
    drawdown(valuations, _ maxByRatio _)

  val start = ((Inflation :: AverageSalary :: simulator.allocation.instruments) map (_.startDate)).max
  val end = ((Inflation :: AverageSalary :: simulator.allocation.instruments) map (_.endDate)).min
  val duration = start.until(end, ChronoUnit.MONTHS).toInt + 1

  private val snapshots = simulator.simulate(start, duration)
  val instalments = snapshots map (s => (s.ym, s.instalment))
  val aggregateInvestment = instalments
    .foldLeft (List((start.minusMonths(1), simulator.initialAmount.toDouble)))
    {case (acc, (ym, instalment)) => (ym, acc.head._2 + instalment) :: acc}
    .reverse drop 1
  println(aggregateInvestment)
  val inflation = instalments
    .foldLeft (List((start.minusMonths(1), simulator.initialAmount.toDouble)))
      {case (acc, (ym, instalment)) => (ym, (acc.head._2 + instalment) * (1 + Inflation.rates(ym))) :: acc}
    .reverse drop 1
  println("Stats inflation:\n" + inflation)
  val portfolioValuations = snapshots map (s => (s.ym, s.value))
  println(portfolioValuations)

  val AbsoluteDrawdown0 = absoluteDrawdown(portfolioValuations, aggregateInvestment)
  val AbsoluteDrawdown1 = absoluteDrawdown(portfolioValuations, inflation)
  val MaximumDrawdown0 = maximumDrawdown(portfolioValuations)
  val RelativeDrawdown0 = relativeDrawdown(portfolioValuations)

  val totalIncome = snapshots map (_.income) sum
  val last12MonthsIncome = (snapshots.reverse take 12 map (_.income)) sum
  val totalInvestment = aggregateInvestment.last._2
}
