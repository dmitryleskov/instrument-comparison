package investment

import java.time.YearMonth

import investment.instruments.Instrument

case class Position(instrument: Instrument,
                    amount: Double) {
  override def toString = instrument + ": %.2fшт.".format(amount)
  def value(ym: YearMonth) = instrument.price(ym) * amount
}

