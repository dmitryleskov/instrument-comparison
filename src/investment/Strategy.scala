package investment

import java.time.YearMonth


abstract class Strategy {
  def portfolioValue(ym: YearMonth, portfolio: Portfolio): Double = {
    (for(Position(instrument, share, amount) <- portfolio) yield instrument.price(ym) * amount).sum
  }

  /** Given a year-month, a portfolio and an amount of money in rubles, invest that amount */
  def invest(ym: YearMonth, portfolio: Portfolio, amount: Double): Portfolio

}

class AllToFirst extends Strategy {
  /** Given a year-month, a portfolio and an amount of money in rubles, invest that amount */
  override def invest(ym: YearMonth, portfolio: Portfolio, cash: Double): Portfolio = {
    val Position(instrument, share, amount) = portfolio.head
    Position(instrument, share, amount + cash / instrument.price(ym)) :: portfolio.tail
  }
}

class EqualAmounts extends Strategy {
  override def invest(ym: YearMonth, portfolio: Portfolio, cash: Double): Portfolio = {
    val cashPerInstrument = cash / portfolio.length
    for (Position(instrument, share, amount) <- portfolio) yield
      Position(instrument, share, amount + cashPerInstrument / instrument.price(ym))
  }
}

class RebalanceMonthly extends Strategy {
  /** Given a year-month, a portfolio and an amount of money in rubles, invest that amount */
  override def invest(ym: YearMonth, portfolio: Portfolio, amount: Double): Portfolio = {
    val average = (portfolioValue(ym, portfolio) + amount) / portfolio.length
    val newPortfolio = for (Position(instrument, share, amount) <- portfolio)
                       yield Position(instrument, share, average / instrument.price(ym))
    //println(ym + " " + portfolioValue(ym, portfolio) + " " + amount + " " + portfolioValue(ym, newPortfolio))
    assert(portfolioValue(ym, portfolio) + amount - portfolioValue(ym, newPortfolio) < 1e-5)
    newPortfolio
  }
}

class BalanceGradually extends Strategy {
  /** Given a year-month, a portfolio and an amount of money in rubles, invest that amount */
  override def invest(ym: YearMonth, portfolio: Portfolio, amount: Double): Portfolio = {
    val values = for(Position(instrument, share, amount) <- portfolio) yield (instrument, instrument.price(ym) * amount)
    val maxValue = (values map (_._2)).max
    val differences = for ((instrument, value) <- values) yield (instrument, maxValue - value)
    val diffTotal = (differences map (_._2)).sum
    val shares = (for((instrument, diff) <- differences) yield
      if (diffTotal < 1e-5)
        (instrument, amount / portfolio.length)
      else
        (instrument, amount * diff / diffTotal)).toMap
    val newPortfolio = for(Position(instrument, share, amount) <- portfolio) yield Position(instrument, share, amount + shares(instrument)/instrument.price(ym))
    //println(ym + " " + portfolioValue(ym, portfolio) + " " + amount + " " + portfolioValue(ym, newPortfolio))
    assert(portfolioValue(ym, portfolio) + amount - portfolioValue(ym, newPortfolio) < 1e-5)
    newPortfolio
  }
}