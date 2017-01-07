/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment

import java.time.YearMonth

import investment.instruments.Instrument

sealed abstract class Strategy (allocation: AssetAllocation) {
  private def assetValues(ym: YearMonth, portfolio: Portfolio): Map[Instrument, Double] =
    (for (position <- portfolio) yield (position.instrument, position.value(ym))).toMap

  private def portfolioValue(ym: YearMonth, portfolio: Portfolio): Double =
    assetValues(ym, portfolio).values.sum

  /* Building blocks: */
  /** The entire instalment always goes to the first instrument (only useful for testing) */
  protected def allToFirst(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio = {
    val Position(instrument, amount) = portfolio.head
    Position(instrument, amount + instalment / instrument.price(ym)) :: portfolio.tail
  }
  /** Simply split instalments according to allocation weights */
  protected def split(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio = {
    val alloc = allocation.allocation(month)
    val denom = alloc.foldLeft(0.0)(_ + _._2)
    for (Position(instrument, amount) <- portfolio) yield
      Position(instrument, amount + (instalment * alloc(instrument)/denom) / instrument.price(ym))
  }
  /** Calculate the current portfolio value, add instalment and redistribute */
  protected def rebalanceAll(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio = {
    val totalValue = portfolioValue(ym, portfolio) + instalment
    val alloc = allocation.allocation(month)
    val denom = alloc.foldLeft(0.0)(_ + _._2)
    val newPortfolio = for (Position(instrument, amount) <- portfolio)
      yield Position(instrument, (totalValue * alloc(instrument)/denom) / instrument.price(ym))
    //println(ym + " " + portfolioValue(ym, portfolio) + " " + amount + " " + portfolioValue(ym, newPortfolio))
    assert(portfolioValue(ym, portfolio) + instalment - portfolioValue(ym, newPortfolio) < 1e-5)
    newPortfolio
  }
  /** Do not sell anything, only distribute the instalment so as to make the allocation closer to desired. */
  protected def distributeInstalment(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio = {
    if (month == 1 || portfolio.length == 1) new Split(allocation).invest(month, ym, portfolio, instalment)
    else {
      val values = assetValues(ym, portfolio)
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
      val (balancer, extra) =
        if (deficit < instalment)
          (deficit, Some(instalment - deficit))
        else
          (instalment, None)

      val interimPortfolio = if (balancer > 0.01) {
        val instalmentAllocation =
          new FixedAllocation((shareDeviations map (d => (d.instrument, Math.sqrt(shareDeviations.max.delta - d.delta)))).toMap)
        new Split(instalmentAllocation).invest(month, ym, portfolio, balancer)
      } else portfolio

      assert(portfolioValue(ym, portfolio) + balancer - portfolioValue(ym, interimPortfolio) < 1e-5)

      val newPortfolio =
        extra match {
          case Some(amount) => split(month, ym, interimPortfolio, amount)
          case None => interimPortfolio
        }
      assert(portfolioValue(ym, portfolio) + instalment - portfolioValue(ym, newPortfolio) < 1e-5)
      newPortfolio
    }
  }
  /** Given a year-month, a portfolio and an amount of money in rubles, invest that amount */
  def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio
}


class AllToFirst(allocation: AssetAllocation) extends Strategy(allocation) {
  override def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio =
    allToFirst(month, ym, portfolio, instalment)
}

class Split(allocation: AssetAllocation) extends Strategy(allocation) {
  override def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio =
    split(month, ym, portfolio, instalment)
}

class BalanceGradually(allocation: AssetAllocation) extends Strategy(allocation) {
  override def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio =
    distributeInstalment(month, ym, portfolio, instalment)
}

class RebalanceMonthly(allocation: AssetAllocation) extends Strategy(allocation) {
  override def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio =
    rebalanceAll(month, ym, portfolio, instalment)
}

class RebalanceQuarterly(allocation: AssetAllocation) extends Strategy(allocation) {
  override def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio =
    if ((month - 1) % 3 == 0)
      rebalanceAll(month, ym, portfolio, instalment)
    else
      distributeInstalment(month, ym, portfolio, instalment)
}

class RebalanceAnnually(allocation: AssetAllocation) extends Strategy(allocation) {
  override def invest(month: Int, ym: YearMonth, portfolio: Portfolio, instalment: Double): Portfolio =
    if ((month - 1) % 12 == 0)
      rebalanceAll(month, ym, portfolio, instalment)
    else
      distributeInstalment(month, ym, portfolio, instalment)
}