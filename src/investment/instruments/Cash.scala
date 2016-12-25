/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment.instruments

import java.time.YearMonth

import investment.data.ExchangeRates

import scala.collection.mutable

sealed abstract class Cash extends Instrument

object Cash {
  private case object RubleCash extends Cash {
    override def toString = "Cash RUB"
    override val endDate = YearMonth.now
    override def price(ym: YearMonth): Double = 1.0
    override def yieldPercentage(ym: YearMonth): Double = 0.0
  }

  private case class ForeignCash(currency: String) extends Cash {
    override val toString = s"Cash $currency"
    private val rates = ExchangeRates(currency)
    override val startDate = rates.startDate
    override val endDate = rates.endDate
    override def price(ym: YearMonth): Double = rates.mid(ym)
    override def yieldPercentage(ym: YearMonth): Double = 0.0
  }

  /** Ruble stands out as being not a foreign currency, hence the need to prepopulate the cache */
  private val cache = mutable.Map[String, Cash]("RUB" -> RubleCash)

  /** @return Cash instance representing the given currency */
  def apply(currency: String): Cash = cache.getOrElseUpdate(currency, new ForeignCash(currency))

  /** @return String identifying the currency of the given Cash instance */
  def unapply(cash: Cash): Option[String] =
  cash match {
    case RubleCash => Some("RUB")
    case ForeignCash(currency) => Some(currency)
  }
}