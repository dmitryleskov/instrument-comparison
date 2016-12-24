package investment

import java.time.temporal.ChronoUnit

import investment.SimulationModel.InstalmentRuleID.{AnnualIncrease, FixedAmount, InflationAdjusted, SalaryPercentage}
import investment.SimulationModel.StrategyID.{BalanceGradually, RebalanceMonthly, Split}
import investment.data.{AverageSalary, Inflation}

import scalafx.beans.property.{IntegerProperty, ObjectProperty, ReadOnlyIntegerProperty, ReadOnlyObjectProperty}

object SimulationModel {
  private val _minYear = IntegerProperty(2006)
  def minYear: ReadOnlyIntegerProperty = _minYear
  private val _maxYear = IntegerProperty(2016)
  def maxYear: ReadOnlyIntegerProperty = _maxYear

  private val _statistics = ObjectProperty[Statistics](this, "statistics")
  val statistics: ReadOnlyObjectProperty[Statistics] = _statistics

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
    case object SalaryPercentage extends InstalmentRuleID {override def toString() = "Salary Percentage"}
    val values = Seq(FixedAmount, AnnualIncrease, InflationAdjusted, SalaryPercentage)
  }

  sealed abstract class InstalmentCurrencyID
  object InstalmentCurrencyID {
    case object RUB extends InstalmentCurrencyID {override def toString() = "RUB"}
    case object USD extends InstalmentCurrencyID {override def toString() = "USD"}
    val values = Seq(RUB, USD)
  }

  val initialAmount = IntegerProperty(0)
  initialAmount.onChange(updateResults)

  val instalmentRuleId = ObjectProperty[InstalmentRuleID](this, "instalmentRule")
  instalmentRuleId.onChange(updateResults)

  val instalmentCurrencyId = ObjectProperty[InstalmentCurrencyID](this, "currency")
  instalmentCurrencyId.onChange(updateResults)

  val initialInstalment = IntegerProperty(0)
  initialInstalment.onChange(updateResults)

  val salaryPercentage = IntegerProperty(10)
  salaryPercentage.onChange(updateResults)

  val allocationProperty = ObjectProperty[AssetAllocation](this, "allocation")
  allocationProperty.onChange(updateResults)

  val strategyId = ObjectProperty[StrategyID](this, "strategy")
  strategyId.onChange(updateResults)

  private def updateResults() = {
    println("Updating results")
    val initialAmount = SimulationModel.initialAmount.value
    val allocation = allocationProperty.value
    if (allocation == null || strategyId.value == null || instalmentRuleId.value == null) {
      _statistics.value = null
    } else {
      val start = ((Inflation :: AverageSalary :: allocation.instruments) map (_.startDate)).max
      val end = ((Inflation :: AverageSalary :: allocation.instruments) map (_.endDate)).min
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
        case SalaryPercentage => new SalaryPercentage(start, salaryPercentage.value.toDouble / 100.0)
        //case ew FixedUSDAmount(start, 10.0)
      }

      val simulator = new Simulator(
        initialAmount,
        allocation,
        instalmentRule,
        strategy)
      val stats  = new Statistics(simulator)
      _statistics.value = stats
      _minYear.value = start.getYear
      _maxYear.value = end.getYear
    }
  }
}
