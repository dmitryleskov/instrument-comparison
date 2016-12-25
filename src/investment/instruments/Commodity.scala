/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment.instruments

import java.time.YearMonth

import investment.data.{CommodityQuotes, ExchangeRates}

case class Commodity(ticker: String) extends Instrument {
  override val toString = ticker
  private val quotes = new CommodityQuotes(ticker)
  private val USDRUB = ExchangeRates("USD")
  override val startDate = quotes.startDate
  override val endDate = quotes.endDate

  /** The price in rubles of purchasing one unit of the given instrument in the given month. */
  override def price(ym: YearMonth): Double = quotes.mid(ym) * USDRUB.mid(ym)
  /** Yield (interest, dividends, etc) of the given instrument in the given month in percents to the price */
  override def yieldPercentage(ym: YearMonth): Double = 0.0
}
