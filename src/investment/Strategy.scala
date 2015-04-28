package investment

import java.time.YearMonth

abstract class Strategy (allocation: AssetAllocation) {
  def portfolioValue(ym: YearMonth, portfolio: Portfolio): Double = {
    (for(Position(instrument, amount) <- portfolio) yield instrument.price(ym) * amount).sum
  }
  /** Given a year-month, a portfolio and an amount of money in rubles, invest that amount */
  def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio
}

/** The entire instalment always goes to the first instument (only useful for testing). */
class AllToFirst(allocation: AssetAllocation) extends Strategy(allocation) {
  override def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio = {
    val Position(instrument, amount) = portfolio.head
    Position(instrument, amount + instalment / instrument.price(ym)) :: portfolio.tail
  }
}

/** Simply split instalments according to allocation factors */
class Split(allocation: AssetAllocation) extends Strategy(allocation) {
  override def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio = {
    val alloc = allocation.allocation(month)
    val denom = alloc.foldLeft(0.0)(_ + _._2)
    for (Position(instrument, amount) <- portfolio) yield
      Position(instrument, amount + (instalment * alloc(instrument)/denom) / instrument.price(ym))
  }
}

/** Calculate current portfolio value, add instalment and redistribute */
class RebalanceMonthly(allocation: AssetAllocation) extends Strategy(allocation) {
  override def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio = {
    val totalValue = portfolioValue(ym, portfolio) + instalment
    val alloc = allocation.allocation(month)
    val denom = alloc.foldLeft(0.0)(_ + _._2)
    val newPortfolio = for (Position(instrument, amount) <- portfolio)
                       yield Position(instrument, (totalValue * alloc(instrument)/denom) / instrument.price(ym))
    //println(ym + " " + portfolioValue(ym, portfolio) + " " + amount + " " + portfolioValue(ym, newPortfolio))
    assert(portfolioValue(ym, portfolio) + instalment - portfolioValue(ym, newPortfolio) < 1e-5)
    newPortfolio
  }
}

/**
 * Never sell anything, only distribute instalments so as to make the allocation closer to desired.
 */
class BalanceGradually(allocation: AssetAllocation) extends Strategy(allocation) {
  override def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio = {
    if (month == 1 || portfolio.length == 1) new Split(allocation).invest(month, ym, portfolio, instalment)
    else {
      val values = (for (position <- portfolio) yield (position.instrument, position.value(ym))).toMap
      val currentAllocation = AssetAllocation.calculate(values)
      val desiredAllocation = allocation.allocation(month)
      case class Deviation(instrument: Instrument, delta: Double) extends Ordered[Deviation] {
        override def compare(that: Deviation): Int = this.delta.compare(that.delta)
      }
      val shareDeviations = for ((instrument, share) <- currentAllocation)
                            yield Deviation(instrument, share - desiredAllocation(instrument))
      val mostOverweight = shareDeviations.max.instrument
      val deficit = values(mostOverweight) / desiredAllocation(mostOverweight) - portfolioValue(ym, portfolio)
      // deficit is how much must be invested in the rest of the portfolio in order to match desiredAllocation

      val instalmentAllocation =
        new FixedAllocation((shareDeviations map (d => (d.instrument, Math.sqrt(shareDeviations.max.delta - d.delta)))).toMap)

      val (balancer, extra) =
        if (deficit < instalment)
          (deficit, Some(instalment - deficit))
        else
          (instalment, None)

      val interimPortfolio = new Split(instalmentAllocation).invest(month, ym, portfolio, balancer)
      assert(portfolioValue(ym, portfolio) + balancer - portfolioValue(ym, interimPortfolio) < 1e-5)

      val newPortfolio =
        extra match {
          case Some(amount) => new Split(allocation).invest(month, ym, interimPortfolio, amount)
          case None => interimPortfolio
        }
      assert(portfolioValue(ym, portfolio) + instalment - portfolioValue(ym, newPortfolio) < 1e-5)
      newPortfolio
    }
  }
}