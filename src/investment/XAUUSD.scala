package investment

import java.time.YearMonth

import scala.collection.mutable

object XAUUSD {
  val (highs, lows) = {
    val dateFormat = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd")
    val csv = new CSVFile("data/" + "XAUUSD.csv")
    val highs = mutable.HashMap[YearMonth, Double]()
    val lows = mutable.HashMap[YearMonth, Double]()

    for (values <- csv) {
      val ym = YearMonth.parse(values(2), dateFormat)
      val open  = values(4).toDouble
      val high  = values(5).toDouble
      val low   = values(6).toDouble
      val close = values(7).toDouble
      println(ym, open, high, low, close)
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
