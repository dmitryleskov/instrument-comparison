package investment

/**
 * Loads all available instruments with historic data
 */
object Instruments {
  val stocks = for (ticker <- Seq("LKOH", "GMKN", "MGNT", "MTSS", "ROSN",
                                  "SBER", "SBERP", "SNGSP", "VTBR")
               ) yield Stock(ticker)
  val usd = new CashUSD
  val deposit = new Deposit(0.10)
  val depositUSD = new DepositUSD(0.03)
  val all: Seq[Instrument] = stocks.asInstanceOf[Seq[Instrument]] :+ usd :+ deposit :+ depositUSD
}
