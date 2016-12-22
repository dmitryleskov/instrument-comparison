package investment

class Statistics(initialAmount: Double, snapshots: List[Snapshot]) {
  val totalIncome = (snapshots map (_.income)) sum
  val last12MonthsIncome = ((snapshots.reverse take 12) map (_.income)) sum
  val totalInvestment = snapshots.foldLeft(initialAmount.toDouble)(_ + _.instalment)
  val maxAbsoluteDrawdown = snapshots.foldLeft((0.0, initialAmount.toDouble)) { case ((d, i), s) =>
    val investedSoFar = i + s.instalment
    (d.min((s.value - investedSoFar) / investedSoFar), investedSoFar)
  } ._1
}
