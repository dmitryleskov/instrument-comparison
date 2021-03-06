/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment.data

import java.time.YearMonth

import investment.util.CSVFile

import scala.collection.mutable

final class ExchangeRates(currency: String) {
  private val (highs, lows) = {
    val dateFormat = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")
    val csv = new CSVFile(s"data/${currency}CB.csv")
    val highs = mutable.HashMap[YearMonth, Double]()
    val lows = mutable.HashMap[YearMonth, Double]()
    for (values  <- csv) {
      val ym = YearMonth.parse(values(0), dateFormat)
      val rate = values(1).toDouble
      highs.get(ym) match {
        case Some(r) => if (rate > r) highs(ym) = rate else ()
        case None => highs(ym) = rate
      }
      lows.get(ym) match {
        case Some(r) => if (rate < r) lows(ym) = rate else ()
        case None => lows(ym) = rate
      }
    }
    (Map() ++ highs, Map() ++ lows)
  }
  def mid(ym: YearMonth) = (highs(ym) + lows(ym)) / 2.0
  val startDate = highs.keys.min
  val endDate = highs.keys.max
}

object ExchangeRates {
  private val cache = mutable.Map[String, ExchangeRates]()
  def apply(currency: String) = cache.getOrElseUpdate(currency, new ExchangeRates(currency))
}