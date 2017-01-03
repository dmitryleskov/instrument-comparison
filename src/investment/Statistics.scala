package investment

import java.time.YearMonth
import java.time.temporal.ChronoUnit

import investment.data.{AverageSalary, Inflation}

import scala.collection.immutable.{IndexedSeq, Seq}

class Statistics(val simulator: Simulator) {
  abstract class Interval
  case object AllTime extends Interval
  case class Years(years: Int) extends Interval
  val intervals = List(1, 3, 5, 10, 15, 20, 30, 40, 50)

  case class Item[T] (best: T,
                      worst: T,
                      median: T,
                      last: T) {
    override def toString = s"Best: $best Worst: $worst Median: $median Last: $last"
  }

  case class Results(interval: Interval,
                     returnOnInvestment0: Item[Double],
                     returnOnInvestment: Item[Double],
                     absoluteDrawdown0: Item[Drawdown],
                     absoluteDrawdown: Item[Drawdown],
                     maximumDrawdown: Item[Drawdown],
                     relativeDrawdown: Item[Drawdown])

  type Z = Map[Interval, Results]

  sealed case class Drawdown(date: YearMonth, amount: Double, ratio: Double) {
    def maxByAmount(other: Drawdown) = if (other.amount > amount) other else this
    def maxByRatio(other: Drawdown) = if (other.ratio > ratio) other else this
    override def toString = if (amount > 0) f"$amount%.2f ${ratio * 100}%.1f%% $date" else "0.0%"
  }
  case object Drawdown {
    val zero = Drawdown(start, 0.0, 0.0)
    /** @return Maximum difference between investment-to-date and current portfolio value */
    private[Statistics] def absolute(valuations: List[(YearMonth, Double)], baseline: List[(YearMonth, Double)]) =
      ((valuations zip baseline) map {
        case ((ymv, v), (ymb, b)) if ymv == ymb => Drawdown(ymv, b - v, (b - v) / b)
      }).foldLeft(Drawdown.zero)(_ maxByAmount _)

    private def drawdown(valuations: List[(YearMonth, Double)], max: (Drawdown, Drawdown) => Drawdown) = {
      val (lastMax, lastMin, lastMinDate, interim) =
        (valuations drop 1).foldLeft(valuations.head._2, valuations.head._2, start, Drawdown.zero) {
          case (prev@(prevMax, prevMin, prevMinDate, res), (ym, v)) =>
            if (v > prevMax)
              (v, v, ym, max(res, Drawdown(prevMinDate, prevMax - prevMin, (prevMax - prevMin) / prevMax)))
            else
              if (v < prevMin) (prevMax, v, ym, res)
            else prev
        }
      if (lastMax > lastMin)
        max(Drawdown(valuations.last._1, lastMax - lastMin, (lastMax - lastMin) / lastMax), interim)
      else interim
    }

    /** @return Maximum difference between a peak and the subsequent trough */
    private[Statistics] def maximum(valuations: List[(YearMonth, Double)]) =
      drawdown(valuations, _ maxByAmount _)

    /** @return Maximum ratio of the difference between a peak and the subsequent trough over the peak */
    private[Statistics] def relative(valuations: List[(YearMonth, Double)]) =
      drawdown(valuations, _ maxByRatio _)
  }

  class InterimResults(val start: YearMonth, val duration: Int) {
    private val snapshots = simulator.simulate(start, duration)
    val instalments: List[(YearMonth, Double)] = snapshots map (s => (s.ym, s.instalment))
    val aggregateInvestment: List[(YearMonth, Double)] =
      instalments
        .foldLeft(List((start.minusMonths(1), simulator.initialAmount.toDouble))) { case (acc, (ym, instalment)) => (ym, acc.head._2 + instalment) :: acc }
        .reverse drop 1
    val inflation: List[(YearMonth, Double)] =
      instalments
        .foldLeft(List((start.minusMonths(1), simulator.initialAmount.toDouble))) { case (acc, (ym, instalment)) => (ym, (acc.head._2 + instalment) * (1 + Inflation.rates(ym))) :: acc }
        .reverse drop 1
    val portfolioValuations: List[(YearMonth, Double)] = snapshots map (s => (s.ym, s.value))

    val totalIncome: Double = snapshots map (_.income) sum
    val last12MonthsIncome: Double = (snapshots.reverse take 12 map (_.income)) sum
    val totalInvestment: Double = aggregateInvestment.last._2

    val returnOnInvestment0: Double = (portfolioValuations.last._2 - totalInvestment) / totalInvestment
    val returnOnInvestment: Double = (portfolioValuations.last._2 - inflation.last._2) / inflation.last._2

    val absoluteDrawdown0: Drawdown = Drawdown.absolute(portfolioValuations, aggregateInvestment)
    val absoluteDrawdown: Drawdown = Drawdown.absolute(portfolioValuations, inflation)
    val maximumDrawdown0: Drawdown = Drawdown.maximum(portfolioValuations)
    val relativeDrawdown0: Drawdown = Drawdown.relative(portfolioValuations)
  }

  val start: YearMonth = ((Inflation :: AverageSalary :: simulator.allocation.instruments) map (_.startDate)).max
  val end: YearMonth = ((Inflation :: AverageSalary :: simulator.allocation.instruments) map (_.endDate)).min
  val allTime: Int = start.until(end, ChronoUnit.MONTHS).toInt + 1
  val allTimeStats = new InterimResults(start, allTime)

  private val resultsByInterval: Map[Int, IndexedSeq[InterimResults]] =
    (intervals map (_ * 12) filter (_ < allTime) map (duration =>
        duration ->
        (for (offset <- 0 to allTime - duration) yield
          new InterimResults(start.plusMonths(offset), duration))
      )
    ).toMap

  val statsByInterval: Map[Int, Item[Drawdown]] = for ((interval, r) <- resultsByInterval) yield
    interval ->
      Item(
        best = r.foldLeft(Drawdown.zero)((acc, r) => if (acc.ratio < r.relativeDrawdown0.ratio) acc else r.relativeDrawdown0),
        worst = r.foldLeft(Drawdown.zero)((acc, r) => if (acc.ratio > r.relativeDrawdown0.ratio) acc else r.relativeDrawdown0),
        median = r.sortBy(_.relativeDrawdown0.ratio).apply(r.length / 2).relativeDrawdown0,
        last = r.last.relativeDrawdown0
      )

}
