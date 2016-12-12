package investment

/**
 * Loads all available instruments with historic data
 */
object Instruments {
  val stocks = for (ticker <- Seq("GAZP", "LKOH", "GMKN", "MGNT", "MOEX", "MTSS",
                                  "ROSN", "SBER", "SBERP", "SNGSP", "VTBR")
               ) yield Stock(ticker)
//  val stocks = for (ticker <- Seq("FXRL", "FXRB", "FXRU", "FXGD")
//               ) yield Stock(ticker)
  val depositUSD = new DepositUSD(0.03)
  val all: Seq[Instrument] =
    stocks.asInstanceOf[Seq[Instrument]] :+ CashUSD :+ CashEUR :+ DepositRUB :+ depositUSD :+ Gold :+ Inflation
}
