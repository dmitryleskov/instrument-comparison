/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment

import investment.data.Inflation

package object instruments {
  val commodities: Seq[Instrument] = for (ticker <- Seq("XAUUSD", "XPTUSD", "XAGUSD", "UKOIL")) yield Commodity(ticker)
  val stocks: Seq[Instrument] = for (ticker <- Seq("ALRS", "GAZP", "LKOH", "NVTK", "GMKN", "MGNT", "MOEX", "MTSS",
    "ROSN", "SBER", "SBERP", "SNGSP", "VTBR")
  ) yield Stock(ticker)
  //  val stocks = for (ticker <- Seq("FXRL", "FXRB", "FXRU", "FXGD")
  //               ) yield Stock(ticker)
  val all: Seq[Instrument] =
    stocks ++
    commodities :+
    Cash("RUB") :+ Cash("USD") :+ Cash("EUR") :+
    Deposit("RUB") :+ Deposit("USD") :+ Deposit("EUR") :+
    Inflation
}
