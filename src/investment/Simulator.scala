/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment

import java.time.YearMonth

import investment.instruments.{Instrument, Stock}

import scala.collection.mutable.ArrayBuffer

case class Snapshot(serial: Int,
                    ym: YearMonth,
                    income: Double,         /* Total amount of dividends, coupons, interest, etc.  received in this month */
                    instalment: Double,
                    portfolio: Portfolio,
                    value: Double)

class Simulator(val initialAmount: Int,
                val allocation: AssetAllocation,
                val rule: InstalmentRule,
                val strategy: Strategy) {
  /**
    * Run simulation defined by class parameters for `length` months
    * starting from `start`
    *
    * @param start First calendar month of simulation
    * @param duration Number of months to simulate, including the `start` month
    * @return A `Seq` of `Snapshots`
    */
  def simulate(start: YearMonth, duration: Int): List[Snapshot] = {
    val snapshots = new Array[Snapshot](duration)
    val templatePortfolio: Portfolio = allocation.instruments map (Position(_, 0))
    var portfolio = new Split(allocation).invest(1, start, templatePortfolio, initialAmount)

    for (month <- 1 to duration) {
      val cur = start.plusMonths(month - 1)
      val instalment = rule.instalment(month)
      val income = (for (Position(instrument, amount) <- portfolio) yield
        instrument.yieldPercentage(cur) * instrument.price(cur) * amount).sum
      portfolio = strategy.invest(month, cur, portfolio, instalment + income) // TODO: simulation without reinvesting
      val value = (for(Position(instrument, amount) <- portfolio) yield instrument.price(cur) * amount).sum
      snapshots(month - 1) = Snapshot(serial=month, cur, income=income, instalment=instalment, portfolio, value=value)
    }
    snapshots.toList
  }
}
