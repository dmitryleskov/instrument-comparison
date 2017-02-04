/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment

import java.time.YearMonth

import investment.data.Inflation

package object instruments {
  val cash: Seq[Cash] = Seq(Cash("RUB"), Cash("USD"), Cash("EUR"))
  val deposits: Seq[Deposit] = Seq(Deposit("RUB"), Deposit("USD"), Deposit("EUR"))
  val commodities: Seq[Commodity] = for (ticker <- Seq("XAUUSD", "XPTUSD", "XAGUSD", "UKOIL")) yield Commodity(ticker)
  val stocks: Seq[Stock] = for (ticker <- Seq("ALRS", "GAZP", "LKOH", "NVTK", "GMKN", "MGNT", "MOEX", "MTSS",
    "ROSN", "SBER", "SBERP", "SNGSP", "VTBR")
  ) yield Stock(ticker)
  val etfs = for (ticker <- Seq("FXMM", /*"FXRL",*/ "FXRB", "FXRU", "FXGD")) yield Stock(ticker)
  val all: Seq[Instrument] = cash ++ deposits ++ commodities ++ stocks ++ etfs :+ Inflation

  /** The earliest date for which data for <em>at least one</em> instrument is available */
  val minStartDate: YearMonth = Seq(Instrument.startDate, (instruments.all map (_.startDate)) min) max
  /** The latest date for which data for <em>all instruments</em> is available */
  val minEndDate: YearMonth = instruments.all map (_.endDate) min
}
