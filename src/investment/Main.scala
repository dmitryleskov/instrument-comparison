package investment

import java.time.YearMonth
import java.time.temporal.ChronoUnit

object Main extends App {
//  val gazp = Stock("GAZP")
  val gmkn = Stock("GMKN")
  val lkoh = Stock("LKOH")
  val mgnt = Stock("MGNT")
  val mtss = Stock("MTSS")
  val rosn = Stock("ROSN")
  val sber = Stock("SBER")
  val sberp = Stock("SBERP")
  val sngsp = Stock("SNGSP")
  val vtbr = Stock("VTBR")
  val usd = new cashUSD
  val deposit = new Deposit(0.10)
  val depositUSD = new DepositUSD(0.03)
  //val singleInvestment = new AnnualIncrease(10.0, 1.05)
  //val singleInvestment = new FixedAmount(1000.0)

  def simulation(instrument: Stock, start: YearMonth, length: Int): Double = {
    var stocks = 0.0
    var dividends = 0.0
    var month = 1
    var cur: YearMonth = start
    val reinvest = true

    while (month <= length) {
      cur = start.plusMonths(month - 1)
      if (month > 3) dividends += instrument.dividends.getOrElse(cur.minusMonths(3), 0.0) * stocks / USDRUB.mid(cur)
      //      stocks += USDRUB.mid(cur) * singleInvestment.instalment(month) / stock.price(cur)
      if (reinvest) {
        stocks += dividends / lkoh.price(cur)
        dividends = 0.0
      }
      month = month + 1

    }
    print(start + " " + dividends + " ")
    stocks * lkoh.price(cur) / USDRUB.mid(cur) + dividends
  }

  //  println("Total investment = " + periods * PeriodicInvestment.investment(1))

  //  val sim = new Simulator(List(stock, usd, deposit), new FixedAmount(1000.0), new AllToFirst)

  val desiredPortfolio: List[(Instrument, Double)] =
//    List(lkoh, usd)
//    List((sngsp, 1.0))
    List((depositUSD, 1.0))
//    List((sber, 1.0), (gmkn, 1.0), (sngsp, 1.0), (lkoh, 1.0), (mtss,1.0))
//    List((sngsp, 1.0), (usd, 1.0))
//    List((depositUSD, 1.0))
//      List(vtbr)
  val span = 120
  val start = (desiredPortfolio map (_._1.startDate)).max
  val end = vtbr.highs.keys.max
  val periods: Int = (start.until(end, ChronoUnit.MONTHS) + 1).toInt

  for (d <- 0 to periods - span) {
    val sim = new Simulator(
      desiredPortfolio,
//      new InflationAdjusted(start.plusMonths(d), 1000.0),
//        new AnnualIncrease(start.plusMonths(d), 1000.0, 1.05),
      new FixedAmount(start.plusMonths(d), 1000.0),
//      new AllToFirst)
//    new EqualAmounts)
      new BalanceGradually)
//      new RebalanceMonthly)
    val end = start.plusMonths(d + span - 1)
    val (totalInvestment, resultingPortfolio) = sim.simulate(start.plusMonths(d), span)
    println(start.plusMonths(d))
    println(resultingPortfolio)
    val value = (resultingPortfolio map {case Position(instrument, _, amount) => amount * instrument.price(end)}).sum
    println("Value = %.2fр.".format(value) +
      " ($%.2f)".format(value/usd.price(end)) +
      " %.2fр. invested".format(totalInvestment) +
      " - %.2fx growth".format(value/totalInvestment))
  }
}
