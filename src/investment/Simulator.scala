package investment

import java.time.YearMonth

import scala.collection.mutable.ArrayBuffer

class Simulator(val allocation: AssetAllocation,
                val rule: InstalmentRule,
                val strategy: Strategy) {
  def simulate(start: YearMonth, length: Int) = {
    var portfolio: Portfolio = for (instrument <- allocation.allocation(1).keys.toList) yield Position(instrument, 0)
    val snapshots = ArrayBuffer[(Int, Double, Portfolio)]()
    var totalInvestment = 0.0
    var totalYield = 0.0
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
      val extra = for(Position(instrument, amount) <- portfolio) yield
        instrument.yieldPercentage(cur) * instrument.price(cur) * amount
      portfolio = strategy.invest(month, cur, portfolio, instalment + extra.sum)
      totalInvestment += instalment
      snapshots += ((month, instalment, portfolio))
      month = month + 1
    }
    snapshots.toList
  }
}
