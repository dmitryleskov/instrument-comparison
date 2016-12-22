package investment

import java.time.YearMonth

import investment.instruments.Instrument

abstract class AssetAllocation {
  // For the given month number (1-based), returns a map of instruments to their desired shares in the portfolio
  def allocation(n: Int): Map[Instrument, Double]
}

object AssetAllocation {
  def calculate(values: Map[Instrument, Double]) = {
    val totalValue = values.values.sum
    values mapValues(_ / totalValue)
  }

  def calculate(ym: YearMonth, portfolio: Portfolio): Map[Instrument, Double] =
    calculate((for (position <- portfolio) yield (position.instrument, position.value(ym))).toMap)

  def normalize(allocation: Map[Instrument, Double]) = {
    val total = allocation.values.sum
    assert(total != 0.0)
    if (total - 1.0 < 1e-10) allocation else allocation mapValues (_ / total)
  }
}

class FixedAllocation (val initialAllocation: Map[Instrument, Double]) extends AssetAllocation {
  lazy val normalized = AssetAllocation.normalize(initialAllocation)
  override def allocation(n: Int): Map[Instrument, Double] = normalized
  override def toString = normalized.toString()
}