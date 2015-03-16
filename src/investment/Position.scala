package investment

case class Position(instrument: Instrument,
                    share: Double,
                    amount: Double) {
  override def toString = instrument + ": %.2fшт.".format(amount)
}

