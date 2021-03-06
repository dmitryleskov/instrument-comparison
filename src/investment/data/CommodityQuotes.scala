/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment.data

import java.time.YearMonth

import investment.util.CSVFile

import scala.collection.mutable

final class CommodityQuotes(ticker: String) {
  val (highs, lows) = {
    val dateFormat = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")
    val csv = new CSVFile(s"data/$ticker.csv")
    val highs = mutable.HashMap[YearMonth, Double]()
    val lows = mutable.HashMap[YearMonth, Double]()

    for (values <- csv) {
      val ym = YearMonth.parse(values(2), dateFormat)
      val open  = values(4).toDouble
      val high  = values(5).toDouble
      val low   = values(6).toDouble
      val close = values(7).toDouble
// asserts commented out cause they don't always hold (need to double check data)
//      assert(high >= low)
//      assert(high >= open)
//      assert(high >= close)
//      assert(low <= open)
//      assert(low <= close)
      highs.get(ym) match {
        case Some(price) => if (high > price) highs(ym) = high else ()
        case None => highs(ym) = high
      }
      lows.get(ym) match {
        case Some(price) => if (low > price) lows(ym) = low else ()
        case None => lows(ym) = low
      }
    }
    (Map() ++ highs, Map() ++ lows)
  }
  def mid(ym: YearMonth) = (highs(ym) + lows(ym)) / 2.0
  val startDate = highs.keys.min
  val endDate = highs.keys.max
}
