package investment

import java.time.YearMonth
import java.time.temporal.ChronoUnit

import investment.Statistics.{AbsoluteDrawdown, BWML, Drawdown, RelativeDrawdown, Results}
import investment.data.{AverageSalary, Inflation}
import investment.instruments.Instrument

import scala.collection.immutable.{IndexedSeq, Seq}

class Statistics(val simulator: Simulator) {
  case class InterimResults(start: YearMonth, duration: Int) {
    private val dateRange: IndexedSeq[YearMonth] = (0 until duration) map { start.plusMonths(_) }
    /** Deflation factors (the trailing 1.0 will become handy when calculating {@code portfolioValues}) */
    private val deflators: Seq[Double] = dateRange
      .map { Inflation.rates(_) }
      .foldRight (List(1.0)) { case (x, acc) => (1.0 + x) * acc.head :: acc }
    /** Raw simulation results */
    private val snapshots = simulator.simulate(start, duration)

    /** Nominal instalments by month */
    val instalments0: List[(YearMonth, Double)] = snapshots map (s => (s.ym, s.instalment))
    /** Inflation-adjusted instalments by month.
      * It is assumed that instalments are made at the beginning of the month, so
      * e.g. the last instalment is adjusted by the last month inflation, and so on. */
    val instalments: List[(YearMonth, Double)] = (instalments0 zip deflators) map { case ((ym, i), d) => (ym, i * d) }

    /** Nominal total investments to date */
    val aggregateInvestment0: List[(YearMonth, Double)] =
      instalments0
        .foldLeft(List((start.minusMonths(1), simulator.initialAmount.toDouble))) { case (acc, (ym, instalment)) => (ym, acc.head._2 + instalment) :: acc }
        .reverse drop 1

    /** Real total investments to date */
    val aggregateInvestment: List[(YearMonth, Double)] =
      instalments
        .foldLeft(List((start.minusMonths(1), simulator.initialAmount.toDouble * deflators.head)))
          { case (acc, (ym, instalment)) => (ym, acc.head._2 + instalment) :: acc }
        .reverse drop 1

    /** Inflation baseline */
    val inflation0: List[(YearMonth, Double)] =
      instalments0
        .foldLeft(List((start.minusMonths(1), simulator.initialAmount.toDouble)))
          { case (acc, (ym, instalment)) => (ym, (acc.head._2 + instalment) * (1 + Inflation.rates(ym))) :: acc }
        .reverse drop 1
    val inflation: List[(YearMonth, Double)] = inflation0 zip (deflators drop 1) map { case ((ym, v), d) => (ym, v * d) }

    /** Nominal values of all assets */
    val portfolioValues0: List[(YearMonth, Double)] = snapshots map (s => (s.ym, s.value))

    /** Real values of all assets.
      * Values are calculated for month <em>ends</em>, so have to be adjusted
      * by <em>next</em> month inflation. Therefore {@code deflators} must contain a trailing 1.0. */
    val portfolioValues: List[(YearMonth, Double)] =
      portfolioValues0 zip (deflators drop 1) map { case ((ym, v), d) => (ym, v * d) }

    val totalIncome: Double = snapshots map (_.income) sum
    val last12MonthsIncome: Double = (snapshots.reverse take 12 map (_.income)) sum
    val totalInvestment: Double = aggregateInvestment0.last._2

    /** Nominal return */
    val returnOnInvestment0: Double = (portfolioValues0.last._2 - totalInvestment) / totalInvestment
    /** Return adjusted for inflation */
    val returnOnInvestment: Double = (portfolioValues0.last._2 - inflation0.last._2) / inflation0.last._2

    val absoluteDrawdown0: AbsoluteDrawdown = Drawdown.absolute(portfolioValues0, aggregateInvestment0)
    val absoluteDrawdown: AbsoluteDrawdown = Drawdown.absolute(portfolioValues0, inflation0)
    val maximumDrawdown0: AbsoluteDrawdown = Drawdown.maximum(portfolioValues0)
    val maximumDrawdown: AbsoluteDrawdown = Drawdown.maximum(portfolioValues)
    val relativeDrawdown0: RelativeDrawdown = Drawdown.relative(portfolioValues0)
    val relativeDrawdown: RelativeDrawdown = Drawdown.relative(portfolioValues)
  }

  val start: YearMonth = ((Inflation :: AverageSalary :: simulator.allocation.instruments) map (_.startDate)).max
  val end: YearMonth = ((Inflation :: AverageSalary :: simulator.allocation.instruments) map (_.endDate)).min
  val duration: Int = start.until(end, ChronoUnit.MONTHS).toInt + 1
  val allTime: InterimResults = InterimResults(start, duration)

  private val resultsByPeriod: Map[Int, IndexedSeq[InterimResults]] =
    (Statistics.periodsOfInterest filter (_ * 12 < duration) map (years =>
        years ->
        (for (offset <- 0 to duration - years * 12) yield
          InterimResults(start.plusMonths(offset), years * 12))
      )
    ).toMap

  def bwml[T](rs: IndexedSeq[InterimResults], getKey: InterimResults => T)(implicit ordering: Ordering[T]): BWML[T] = {
    val biggerIsBetter = false
    val s = rs.sorted(ordering.on(getKey))
    BWML(
      best = getKey(if (biggerIsBetter) s.last else s.head),
      worst = getKey(if (biggerIsBetter) s.head else s.last),
      median = getKey(s.apply(s.length / 2)),
      last = getKey(rs.last)
    )
  }

  val byPeriod: Map[Int, Results] =
    for ((period, r) <- resultsByPeriod) yield
      period ->
        Results(
          returnOnInvestment0 = bwml(r, _.returnOnInvestment0),
          returnOnInvestment = bwml(r, _.returnOnInvestment),
          absoluteDrawdown0 = bwml(r, _.absoluteDrawdown0),
          absoluteDrawdown = bwml(r, _.absoluteDrawdown),
          maximumDrawdown0 = bwml(r, _.maximumDrawdown0),
          maximumDrawdown = bwml(r, _.maximumDrawdown),
          relativeDrawdown0 = bwml(r, _.relativeDrawdown0),
          relativeDrawdown = bwml(r, _.relativeDrawdown)
        )
}

object Statistics {
  case class BWML[+T](best: T,
                      worst: T,
                      median: T,
                      last: T) {
    override def toString = s"Best: $best\nWorst: $worst\nMedian: $median\nLast: $last"
  }

  case class Results(returnOnInvestment0: BWML[Double],
                     returnOnInvestment: BWML[Double],
                     absoluteDrawdown0: BWML[AbsoluteDrawdown],
                     absoluteDrawdown: BWML[AbsoluteDrawdown],
                     maximumDrawdown0: BWML[AbsoluteDrawdown],
                     maximumDrawdown: BWML[AbsoluteDrawdown],
                     relativeDrawdown0: BWML[RelativeDrawdown],
                     relativeDrawdown: BWML[RelativeDrawdown]) {
    override def toString =
      s"""|Return on Investment:
          |$returnOnInvestment0
          |
          |Return on Investment (Inflation-Adjusted):
          |$returnOnInvestment
          |
          |Absolute Drawdown:
          |$absoluteDrawdown0
          |
          |Absolute Drawdown (Inflation-Adjusted):
          |$absoluteDrawdown
          |
          |Maximum Drawdown:
          |$maximumDrawdown0
          |
          |Maximum Drawdown (Inflation-Adjusted):
          |$maximumDrawdown
          |
          |Relative Drawdown:
          |$relativeDrawdown0
          |
          |Relative Drawdown (Inflation-Adjusted):
          |$relativeDrawdown""".stripMargin
  }

  abstract class Drawdown

  trait DrawdownOps[D <: Drawdown] extends Ordering[D] {
    def zero: D
    def build(ym: YearMonth, max: Double, min: Double): D
  }

  case class AbsoluteDrawdown(date: YearMonth, amount: Double, ratio: Double) extends Drawdown {
    override def toString = if (amount > 0) f"$amount%.2f (${ratio * 100}%.1f%%) $date" else "0.00"
  }

  implicit object AbsoluteDrawdownOps extends DrawdownOps[AbsoluteDrawdown] {
    override def compare(x: AbsoluteDrawdown, y: AbsoluteDrawdown): Int = x.amount.compare(y.amount)
    implicit def zero: AbsoluteDrawdown = AbsoluteDrawdown(Instrument.startDate, 0.0, 0.0)
    implicit def build(minDate: YearMonth, max: Double, min: Double): AbsoluteDrawdown =
      AbsoluteDrawdown(minDate, max - min, (max - min) / max)
  }

  case class RelativeDrawdown(date: YearMonth, ratio: Double, amount: Double) extends Drawdown {
    override def toString = if (ratio > 0) f"${ratio * 100}%.1f%% ($amount%.2f) $date" else "0.0%"
  }

  implicit object RelativeDrawdownOps extends DrawdownOps[RelativeDrawdown] {
    override def compare(x: RelativeDrawdown, y: RelativeDrawdown): Int = x.ratio.compare(y.ratio)
    implicit def zero: RelativeDrawdown = RelativeDrawdown(Instrument.startDate, 0.0, 0.0)
    implicit def build(minDate: YearMonth, max: Double, min: Double): RelativeDrawdown =
      RelativeDrawdown(minDate, (max - min) / max, max - min)
  }

  object Drawdown {
    /** @return Maximum difference between investment-to-date and current portfolio value */
    private[Statistics] def absolute(valuations: List[(YearMonth, Double)], baseline: List[(YearMonth, Double)])
                                    (implicit ops: DrawdownOps[AbsoluteDrawdown])=
      ((valuations zip baseline) map {
        case ((ymv, v), (ymb, b)) if ymv == ymb => ops.build(ymv, b, v)
      }).foldLeft(ops.zero)(ops.max)

    private def drawdown[T <: Drawdown](valuations: List[(YearMonth, Double)])
                                       (implicit ops: DrawdownOps[T])= {
      val (lastMax, lastMin, lastMinDate, interim) =
        (valuations drop 1).foldLeft(valuations.head._2, valuations.head._2, valuations.head._1, ops.zero) {
          case (prev@(prevMax, prevMin, prevMinDate, res), (ym, v)) =>
            if (v > prevMax)
              (v, v, ym, ops.max(res, ops.build(prevMinDate, prevMax, prevMin)))
            else
            if (v < prevMin) (prevMax, v, ym, res)
            else prev
        }
      if (lastMax > lastMin)
        ops.max(interim, ops.build(valuations.last._1, lastMax, lastMin))
      else interim
    }

    /** @return Maximum difference between a peak and the subsequent trough */
    private[Statistics] def maximum(valuations: List[(YearMonth, Double)]) =
      drawdown[AbsoluteDrawdown](valuations)

    /** @return Maximum ratio of the difference between a peak and the subsequent trough over the peak */
    private[Statistics] def relative(valuations: List[(YearMonth, Double)]) =
      drawdown[RelativeDrawdown](valuations)
  }

  val periodsOfInterest = List(1, 3, 5, 10, 15, 20, 30, 40, 50, 60, 75, 100) filter (Instrument.startDate.plusYears(_).isBefore(YearMonth.now()))
}