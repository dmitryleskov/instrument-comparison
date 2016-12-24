package investment

import java.time.YearMonth
import java.time.temporal.ChronoUnit

import investment.data.{AverageSalary, Inflation}

class Statistics(val simulator: Simulator) {
  private def maxAbsoluteDrawdown(valuations: List[(YearMonth, Double)], baseline: List[(YearMonth, Double)]) =
    ((valuations zip baseline) map {
      case ((ymv, v), (ymb, b)) if ymv == ymb => v / b
    }).foldLeft(1.0)(_ min _) - 1.0

  private def maxRelativeDrawdown(valuations: List[(YearMonth, Double)]) =
    (valuations drop 1) .foldLeft (valuations.head._2, valuations.head._2, 0.0) {
      case ((lastMax, lastMin, maxDrawdown), (ym, v)) =>
        if (v > lastMax) {
          (v, v, maxDrawdown.min((lastMin - lastMax) / lastMax))
        } else {
          (lastMax, lastMin min v, maxDrawdown)
        }
    }._3

  val start = ((Inflation :: AverageSalary :: simulator.allocation.instruments) map (_.startDate)).max
  val end = ((Inflation :: AverageSalary :: simulator.allocation.instruments) map (_.endDate)).min
  val duration = start.until(end, ChronoUnit.MONTHS).toInt + 1

  private val snapshots = simulator.simulate(start, duration)
  println(snapshots take 2)
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

  val maxAbsoluteDrawdown0 = maxAbsoluteDrawdown(portfolioValuations, aggregateInvestment)
  val maxAbsoluteDrawdown1 = maxAbsoluteDrawdown(portfolioValuations, inflation)
  val maxRelativeDrawdown0 = maxRelativeDrawdown(portfolioValuations)

  println((maxAbsoluteDrawdown0 * 100.0).formatted("%.1f%%"))
  println((maxRelativeDrawdown0 * 100.0).formatted("%.1f%%"))

  val totalIncome = snapshots map (_.income) sum
  val last12MonthsIncome = (snapshots.reverse take 12 map (_.income)) sum
  val totalInvestment = aggregateInvestment.last._2
}
