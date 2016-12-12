package investment

import java.time.temporal.ChronoUnit

import investment.SimulationModel.InstalmentRuleID.{InflationAdjusted, AnnualIncrease, FixedAmount}
import investment.SimulationModel.StrategyID.{RebalanceMonthly, BalanceGradually, Split}

import scalafx.beans.property.{ObjectProperty, IntegerProperty}
import scalafx.collections.ObservableBuffer

object SimulationModel {
  val results = new ObservableBuffer[Snapshot]()
  val inflation = new ObservableBuffer[(Snapshot)]()
  val portfolioValues = new ObservableBuffer[(Int, Double)]

  sealed abstract class StrategyID
  object StrategyID {
    case object Split extends StrategyID {override def toString() = "Simple Split"}
    case object BalanceGradually extends StrategyID {override def toString() = "Balance Gradually"}
    case object RebalanceMonthly extends StrategyID {override def toString() = "Rebalance Monthly"}
    val values = Seq(Split, BalanceGradually, RebalanceMonthly)
  }

  sealed abstract class InstalmentRuleID
  object InstalmentRuleID {
    case object FixedAmount extends InstalmentRuleID {override def toString() = "Fixed Amount"}
    case object AnnualIncrease extends InstalmentRuleID {override def toString() = "Annual Increase"}
    case object InflationAdjusted extends InstalmentRuleID {override def toString() = "Inflation Adjusted"}
    val values = Seq(FixedAmount, AnnualIncrease, InflationAdjusted)
  }

  sealed abstract class InstalmentCurrencyID
  object InstalmentCurrencyID {
    case object RUB extends InstalmentCurrencyID {override def toString() = "RUB"}
    case object USD extends InstalmentCurrencyID {override def toString() = "USD"}
    val values = Seq(RUB, USD)
  }

  val initialInstalment = IntegerProperty(0) // FIXME
  initialInstalment.onChange(updateResults)

  val allocationProperty = ObjectProperty[AssetAllocation](this, "allocation")
  allocationProperty.onChange(updateResults)

  val strategyId = ObjectProperty[StrategyID](this, "strategy")
  strategyId.onChange(updateResults)

  val instalmentRuleId = ObjectProperty[InstalmentRuleID](this, "instalmentRule")
  instalmentRuleId.onChange(updateResults)

  val instalmentCurrencyId = ObjectProperty[InstalmentCurrencyID](this, "currency")
  instalmentCurrencyId.onChange(updateResults)

  def updateResults() = {
    println("Updating results")
    val allocation = allocationProperty.value
    println(allocation)

    if (allocation == null || strategyId.value == null) {
      results.clear()
      portfolioValues.clear()
    } else {
      val start = (((Inflation, 1.0) :: allocation.allocation(1).toList) map (_._1.startDate)).max
      val end = (((Inflation, 1.0) :: allocation.allocation(1).toList) map (_._1.endDate)).min
      println(Inflation.endDate)
      println(start, end)
      val strategy = strategyId.value match {
        case Split => new Split(allocation)
        case BalanceGradually => new BalanceGradually(allocation)
        case RebalanceMonthly => new RebalanceMonthly(allocation)
      }

      val instalmentRule = instalmentRuleId.value match {
        case FixedAmount => new FixedAmount(start, initialInstalment.value)
        case AnnualIncrease => new AnnualIncrease(start, initialInstalment.value, 1.05)
        case InflationAdjusted => new InflationAdjusted(start, initialInstalment.value)
        //case ew FixedUSDAmount(start, 10.0)
        //        new InflationAdjusted(start, 1000.0)
      }


      val sim = new Simulator(
        allocation,
        instalmentRule,
        strategy)

      val inflationAllocation = new FixedAllocation(Map(Inflation -> 1.0))
      val inflationSim = new Simulator(
        inflationAllocation,
        instalmentRule,
        new Split(inflationAllocation))

      results.setAll(sim.simulate(start, start.until(end, ChronoUnit.MONTHS).toInt + 1): _*)

      portfolioValues.setAll(results map (x => (x.serial, x.value)))

      inflation.setAll(inflationSim.simulate(start, start.until(end, ChronoUnit.MONTHS).toInt + 1): _*)
      println(inflation)
    }
  }
}
