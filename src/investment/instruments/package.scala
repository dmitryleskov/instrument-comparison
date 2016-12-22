package investment

import investment.data.Inflation

package object instruments {
  val stocks = for (ticker <- Seq("ALRS", "GAZP", "LKOH", "NVTK", "GMKN", "MGNT", "MOEX", "MTSS",
    "ROSN", "SBER", "SBERP", "SNGSP", "VTBR")
  ) yield Stock(ticker)
  //  val stocks = for (ticker <- Seq("FXRL", "FXRB", "FXRU", "FXGD")
  //               ) yield Stock(ticker)
  val depositUSD = new DepositUSD(0.03)
  val all: Seq[Instrument] =
    stocks.asInstanceOf[Seq[Instrument]] :+ CashRUB :+ CashUSD :+ CashEUR :+ DepositRUB :+ depositUSD :+ Gold :+ Inflation
}
