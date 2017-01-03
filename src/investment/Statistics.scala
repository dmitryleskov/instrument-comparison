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
                     absoluteDrawdown0: Item[AbsoluteDrawdown],
                     absoluteDrawdown: Item[AbsoluteDrawdown],
                     maximumDrawdown: Item[AbsoluteDrawdown],
                     relativeDrawdown: Item[RelativeDrawdown])

  type Z = Map[Interval, Results]

  trait HasZero[T] {
    def zero: T
  }
  trait CanBuildFrom3[-T1, -T2, -T3, +U] {
    def build(t1: T1, t2: T2, t3: T3): U
  }

  abstract class Drawdown

  case class AbsoluteDrawdown(date: YearMonth, amount: Double, ratio: Double) extends Drawdown {
    override def toString = if (amount > 0) f"$amount%.2f (${ratio * 100}%.1f%%) $date" else "0.00"
  }
  implicit object AbsoluteDrawdownOrdering extends Ordering[AbsoluteDrawdown] {
    override def compare(x: AbsoluteDrawdown, y: AbsoluteDrawdown): Int = x.amount.compare(y.amount)
  }
  implicit object AbsoluteDrawdownHasZero extends HasZero[AbsoluteDrawdown] {
    implicit def zero: AbsoluteDrawdown = AbsoluteDrawdown(start, 0.0, 0.0)
  }
  implicit object AbsoluteDrawdownBuilder extends CanBuildFrom3[YearMonth, Double, Double, AbsoluteDrawdown] {
    implicit def build(minDate: YearMonth, max: Double, min: Double): AbsoluteDrawdown = 
      AbsoluteDrawdown(minDate, max - min, (max - min) / max)
  }
  
  case class RelativeDrawdown(date: YearMonth, ratio: Double, amount: Double) extends Drawdown {
    override def toString = if (ratio > 0) f"${ratio * 100}%.1f%% ($amount%.2f) $date" else "0.0%"
  }
  implicit object RelativeDrawdownOrdering extends Ordering[RelativeDrawdown] {
    override def compare(x: RelativeDrawdown, y: RelativeDrawdown): Int = x.ratio.compare(y.ratio)
  }
  implicit object RelativeDrawdownHasZero extends HasZero[RelativeDrawdown] {
    implicit def zero: RelativeDrawdown = RelativeDrawdown(start, 0.0, 0.0)
  }
  implicit object RelativeDrawdownBuilder extends CanBuildFrom3[YearMonth, Double, Double, RelativeDrawdown] {
    implicit def build(minDate: YearMonth, max: Double, min: Double): RelativeDrawdown =
      RelativeDrawdown(minDate, (max - min) / max, max - min)
  }
  
  object Drawdown {
    /** @return Maximum difference between investment-to-date and current portfolio value */
    private[Statistics] def absolute(valuations: List[(YearMonth, Double)], baseline: List[(YearMonth, Double)])
                                    (implicit ordering: Ordering[AbsoluteDrawdown])=
      ((valuations zip baseline) map {
        case ((ymv, v), (ymb, b)) if ymv == ymb => AbsoluteDrawdown(ymv, b, v)
      }).foldLeft(AbsoluteDrawdownHasZero.zero)(ordering.max)

    private def drawdown[T <: Drawdown](valuations: List[(YearMonth, Double)])
                           (implicit ordering: Ordering[T], hasZero: HasZero[T], builder: CanBuildFrom3[YearMonth, Double, Double, T])= {
      val (lastMax, lastMin, lastMinDate, interim) =
        (valuations drop 1).foldLeft(valuations.head._2, valuations.head._2, start, hasZero.zero) {
          case (prev@(prevMax, prevMin, prevMinDate, res), (ym, v)) =>
            if (v > prevMax)
              (v, v, ym, ordering.max(res, builder.build(prevMinDate, prevMax, prevMin)))
            else
              if (v < prevMin) (prevMax, v, ym, res)
            else prev
        }
      if (lastMax > lastMin)
        ordering.max(interim, builder.build(valuations.last._1, lastMax, lastMin))
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
    (intervals map (_ * 12) filter (_ < allTime) map (duration =>
        duration ->
        (for (offset <- 0 to allTime - duration) yield
          new InterimResults(start.plusMonths(offset), duration))
      )
    ).toMap

  val statsByInterval: Map[Int, Item[RelativeDrawdown]] = for ((interval, r) <- resultsByInterval) yield
    interval ->
      Item(
        best = r.foldLeft(RelativeDrawdownHasZero.zero)((acc, r) => if (acc.ratio < r.relativeDrawdown0.ratio) acc else r.relativeDrawdown0),
        worst = r.foldLeft(RelativeDrawdownHasZero.zero)((acc, r) => if (acc.ratio > r.relativeDrawdown0.ratio) acc else r.relativeDrawdown0),
        median = r.sortBy(_.relativeDrawdown0.ratio).apply(r.length / 2).relativeDrawdown0,
        last = r.last.relativeDrawdown0
      )

}
