package investment

import java.time.YearMonth

class Simulator(val desiredPortfolio: List[(Instrument, Double)],
                val rule: InstalmentRule,
                val strategy: Strategy) {
  def simulate(start: YearMonth, length: Int) = {
    var portfolio: Portfolio = for ((instrument, share) <- desiredPortfolio) yield Position(instrument, share, 0)
    var totalInvestment = 0.0
    var totalYield = 0.0
    var month = 1
    var cur: YearMonth = start
    val reinvest = true
    val allSplits = (for (Position(stock: Stock, _, _) <- portfolio;
                          (date, factor) <- stock.splits)
                     yield date -> (stock, factor)
                    ) groupBy (_._1)

    while (month <= length) {
      cur = start.plusMonths(month - 1)
      if (allSplits contains cur) {
        // Map[Stock, Double] inferred by default, so need explicit type
        val splits: Map[Instrument, Double] = allSplits(cur).toMap.values.toMap
        portfolio = for (Position(instrument, share, amount) <- portfolio)
                    yield Position(instrument, share, amount * splits.getOrElse(instrument, 1.0))
      }
      val instalment = rule.instalment(month)
      val extra = for(Position(instrument, share, amount) <- portfolio) yield
        instrument.yieldPercentage(cur) * instrument.price(cur) * amount
//      println( extra)
      portfolio = strategy.invest(cur, portfolio, instalment+extra.sum)
      totalInvestment += instalment
      /*
            if (reinvest) {
              stocks += dividends / stock.price(cur); dividends = 0.0
            }
      */
      month = month + 1
    }
    (totalInvestment, portfolio)
  }

}
