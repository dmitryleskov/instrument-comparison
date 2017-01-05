/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment.instruments

import java.time.YearMonth
import java.util.Locale

import investment.data.ExchangeRates
import investment.util.CSVFile

import scala.collection.mutable

sealed abstract class Deposit(val currency: String) extends Instrument {
  override val toString = s"Deposit $currency"

  protected val interest: Map[YearMonth, Double] = {
    val dateFormat = java.time.format.DateTimeFormatter.ofPattern("MMM-yy", Locale.US)
    val csv = new CSVFile(s"data/Deposit-$currency.csv")
    (for (values <- csv) yield YearMonth.parse(values(0), dateFormat) -> values(1).toDouble / 100).toMap
  }
  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to its value */
  override def yieldPercentage(ym: YearMonth): Double = math.pow(1.0 + interest(ym), 1.0 / 12.0) - 1.0
}

object Deposit {
  case object DepositRUB extends Deposit("RUB") {
    override lazy val startDate = interest.keys.min
    override lazy val endDate = interest.keys.max
    /** The price in rubles of purchasing one unit of the given instrument in the given month. */
    override def price(ym: YearMonth): Double = 1.0
  }

  case class DepositOther(override val currency: String) extends Deposit(currency) {
    private val rates = ExchangeRates(currency)
    override lazy val startDate = List(rates.startDate.plusMonths(1), interest.keys.min).max
    override lazy val endDate = List(rates.endDate, interest.keys.max).min

    /** The price in rubles of purchasing one unit of the given instrument in the given month.
      *
      * @param ym
      * @return the midpoint CBR rate for the month
      */
    override def price(ym: YearMonth): Double = rates.mid(ym)
  }

  def apply(currency: String): Deposit =
    currency match {
      case "RUB" => DepositRUB
      case other => DepositOther(other)
    }
  def unapply(deposit: Deposit) = Some(deposit.currency)
}


