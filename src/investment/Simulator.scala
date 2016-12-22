package investment

import java.time.YearMonth

import scala.collection.mutable.ArrayBuffer

case class Snapshot(serial: Int,
                    ym: YearMonth,
                    income: Double,         /* Total amount of dividends, coupons, interest, etc.  received*/
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
    * @param start
    * @param length
    * @return A `Seq` of `(Int, Double, Portfolio)` representing month number,
    *         instalment added and resulting asset allocation
    */
  def simulate(start: YearMonth, length: Int): List[Snapshot] = {
    val templatePortfolio: Portfolio = for (instrument <- allocation.allocation(1).keys.toList) yield Position(instrument, 0)
    var portfolio = new Split(allocation).invest(1, start, templatePortfolio, initialAmount)
    val snapshots = ArrayBuffer[Snapshot]()
    var month = 1
    var cur: YearMonth = start
    val reinvest = true
    val allSplits = (for (Position(stock: Stock, _) <- portfolio;
                          (date, factor) <- stock.splits)
                     yield date -> (stock, factor)
                    ) groupBy (_._1)

    while (month <= length) {
      cur = start.plusMonths(month - 1)
      if (allSplits contains cur) {
        // Map[Stock, Double] inferred by default, so need explicit type
        val splits: Map[Instrument, Double] = allSplits(cur).toMap.values.toMap
        portfolio = for (Position(instrument, amount) <- portfolio)
                    yield Position(instrument, amount * splits.getOrElse(instrument, 1.0))
      }
      val instalment = rule.instalment(month)
      val income = (for (Position(instrument, amount) <- portfolio) yield
        instrument.yieldPercentage(cur) * instrument.price(cur) * amount).sum
      portfolio = strategy.invest(month, cur, portfolio, instalment + income)
      val value = (for(Position(instrument, amount) <- portfolio) yield instrument.price(cur) * amount).sum
      snapshots += Snapshot(serial=month, cur, income, instalment, portfolio, value)
      month = month + 1
    }
    snapshots.toList
  }
}
