package investment

import java.time.YearMonth
import java.time.temporal.ChronoUnit

import investment.instruments._

object Test extends App {
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
  val usd = CashUSD
  val deposit = DepositRUB
  val depositUSD = new DepositUSD(0.03)

  val allocationDescriptors: List[List[(Instrument, Double)]] =
    List(
//      List((sngsp, 1.0)),
//      List((depositUSD, 1.0)),
      List((sber, 1.0), (gmkn, 1.0), (sngsp, 1.0), (lkoh, 1.0), (mtss, 1.0)),
      List((sngsp, 1.0), (depositUSD, 1.0))
    )
  val allocations = allocationDescriptors map (x => new FixedAllocation(x.toMap))
  val span = 120

  val start = (allocationDescriptors.flatten map (_._1.startDate)).max
  println(start)
  val end = vtbr.high.keys.max
  val periods: Int = (start.until(end, ChronoUnit.MONTHS) + 1).toInt

  for (d <- 0 to periods - span) {
    println("Start: " + start.plusMonths(d))
    for (allocation <- allocations) {
      val sim = new Simulator(
        0,
        allocation,
        //      new InflationAdjusted(start.plusMonths(d), 1000.0),
        //        new AnnualIncrease(start.plusMonths(d), 1000.0, 1.05),
        new FixedAmount(start.plusMonths(d), 1000.0),
        //      new AllToFirst)
      //  new Split(allocation))
            new BalanceGradually(allocation))
      //      new RebalanceMonthly(allocation))
      val end = start.plusMonths(d + span - 1)
      val res = sim.simulate(start.plusMonths(d), span)
      val totalInvestment = (res map (_.instalment)).sum
      println(res.last.portfolio)
      val value = (res.last.portfolio map { case Position(instrument, amount) => amount * instrument.price(end)}).sum
      println("Value = %.2fр.".format(value) +
        " ($%.2f)".format(value / usd.price(end)) +
        " %.2fр. invested".format(totalInvestment) +
        " - %.2fx growth".format(value / totalInvestment))
    }
  }
}
