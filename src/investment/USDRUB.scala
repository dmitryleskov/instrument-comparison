package investment

import java.time.YearMonth

import scala.collection.mutable

object USDRUB {
  val dateFormat = java.time.format.DateTimeFormatter.ofPattern("d.MM.yyyy")
  val csv = new CSVFile("data/" + "RUBUSD.csv")
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
  println(highs)
  println(lows)
  val h = Map() ++ highs
  def mid(ym: YearMonth) = (highs(ym) + lows(ym)) / 2.0
}
