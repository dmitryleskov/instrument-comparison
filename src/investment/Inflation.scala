package investment

import java.time.{Year, YearMonth}
import scala.collection.mutable

object Inflation {
  val annual: Map[Year, Double] = {
    val dateFormat = java.time.format.DateTimeFormatter.ofPattern("yyyy")
    val csv = new CSVFile("data/Inflation.csv")
    val data = mutable.HashMap[Year, Double]()
    for (values <- csv) {
      val year: Year = Year.parse(values(0), dateFormat)
      val rate = values(1).toDouble
      data(year) = rate
    }
    Map() ++ data
  }
  def aggregate(from: YearMonth, to: YearMonth): Double = {
    if (from.getYear == to.getYear)
      annual(Year.from(from))
    else
      annual(Year.from(to)) * aggregate(from, to.minusYears(1))
  }
}
