package investment

import java.time.YearMonth
import java.time.temporal.ChronoUnit

import investment.data.{AverageSalary, Inflation}
import investment.instruments.Instrument

import scala.collection.immutable.{IndexedSeq, Seq}

class Statistics(val simulator: Simulator) {

  case class Item[T](best: T,
                     worst: T,
                     median: T,
                     last: T) {
    override def toString = s"Best: $best\nWorst: $worst\nMedian: $median\nLast: $last"
  }

  case class Results(returnOnInvestment0: Item[Double],
                     returnOnInvestment: Item[Double],
                     absoluteDrawdown0: Item[AbsoluteDrawdown],
                     absoluteDrawdown: Item[AbsoluteDrawdown],
                     maximumDrawdown0: Item[AbsoluteDrawdown],
                     relativeDrawdown0: Item[RelativeDrawdown]) {
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
          |Relative Drawdown:
          |$relativeDrawdown0""".stripMargin
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
    implicit def zero: AbsoluteDrawdown = AbsoluteDrawdown(start, 0.0, 0.0)
    implicit def build(minDate: YearMonth, max: Double, min: Double): AbsoluteDrawdown =
      AbsoluteDrawdown(minDate, max - min, (max - min) / max)
  }

  case class RelativeDrawdown(date: YearMonth, ratio: Double, amount: Double) extends Drawdown {
    override def toString = if (ratio > 0) f"${ratio * 100}%.1f%% ($amount%.2f) $date" else "0.0%"
  }

  implicit object RelativeDrawdownOps extends DrawdownOps[RelativeDrawdown] {
    override def compare(x: RelativeDrawdown, y: RelativeDrawdown): Int = x.ratio.compare(y.ratio)
    implicit def zero: RelativeDrawdown = RelativeDrawdown(start, 0.0, 0.0)
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
        (valuations drop 1).foldLeft(valuations.head._2, valuations.head._2, start, ops.zero) {
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

    val absoluteDrawdown0: AbsoluteDrawdown = Drawdown.absolute(portfolioValuations, aggregateInvestment)
    val absoluteDrawdown: AbsoluteDrawdown = Drawdown.absolute(portfolioValuations, inflation)
    val maximumDrawdown0: AbsoluteDrawdown = Drawdown.maximum(portfolioValuations)
    val relativeDrawdown0: RelativeDrawdown = Drawdown.relative(portfolioValuations)
  }

  val start: YearMonth = ((Inflation :: AverageSalary :: simulator.allocation.instruments) map (_.startDate)).max
  val end: YearMonth = ((Inflation :: AverageSalary :: simulator.allocation.instruments) map (_.endDate)).min
  val allTime: Int = start.until(end, ChronoUnit.MONTHS).toInt + 1
  val allTimeStats = new InterimResults(start, allTime)

  private val resultsByInterval: Map[Int, IndexedSeq[InterimResults]] =
    (Statistics.intervals filter (_ * 12 < allTime) map (years =>
        years ->
        (for (offset <- 0 to allTime - years * 12) yield
          new InterimResults(start.plusMonths(offset), years * 12))
      )
    ).toMap

  def compute[T](rs: IndexedSeq[InterimResults], getKey: InterimResults => T)(implicit ordering: Ordering[T]): Item[T] = {
    val biggerIsBetter = false
    val s = rs.sorted(ordering.on(getKey))
    Item(
      best = getKey(if (biggerIsBetter) s.last else s.head),
      worst = getKey(if (biggerIsBetter) s.head else s.last),
      median = getKey(s.apply(s.length / 2)),
      last = getKey(rs.last)
    )
  }

  val statsByInterval: Map[Int, Results] =
    for ((interval, r) <- resultsByInterval) yield
      interval ->
        Results(
          returnOnInvestment0 = compute(r, _.returnOnInvestment0),
          returnOnInvestment = compute(r, _.returnOnInvestment),
          absoluteDrawdown0 = compute(r, _.absoluteDrawdown0),
          absoluteDrawdown = compute(r, _.absoluteDrawdown),
          maximumDrawdown0 = compute(r, _.maximumDrawdown0),
          relativeDrawdown0 = compute(r, _.relativeDrawdown0)
        )
}

object Statistics {
  val intervals = List(1, 3, 5, 10, 15, 20, 30, 40, 50, 60, 75, 100) filter (Instrument.startDate.plusYears(_).isBefore(YearMonth.now()))
}