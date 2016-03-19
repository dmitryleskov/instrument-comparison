package investment

/**
 * Loads all available instruments with historic data
 */
object Instruments {
  val stocks = for (ticker <- Seq("LKOH", "GMKN", "MGNT", "MTSS", "ROSN",
                                  "SBER", "SBERP", "SNGSP", "VTBR")
               ) yield Stock(ticker)
  val depositUSD = new DepositUSD(0.03)
  val all: Seq[Instrument] =
    stocks.asInstanceOf[Seq[Instrument]] :+ CashUSD :+ DepositRUB :+ depositUSD :+ Inflation
}
