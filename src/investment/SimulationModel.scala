package investment

import java.time.temporal.ChronoUnit
import javafx.beans.property.ReadOnlyStringProperty

import investment.SimulationModel.InstalmentRuleID.{AnnualIncrease, FixedAmount, InflationAdjusted, SalaryPercentage}
import investment.SimulationModel.StrategyID.{BalanceGradually, RebalanceMonthly, Split}
import investment.data.{AverageSalary, Inflation}

import scalafx.beans.property.{IntegerProperty, ObjectProperty, ReadOnlyIntegerProperty, StringProperty}
import scalafx.collections.ObservableBuffer

object SimulationModel {
  private val _minYear = IntegerProperty(2006)
  def minYear: ReadOnlyIntegerProperty = _minYear
  private val _maxYear = IntegerProperty(2016)
  def maxYear: ReadOnlyIntegerProperty = _maxYear
  private val _summary = StringProperty("")
  def summary: ReadOnlyStringProperty = _summary
  val snapshots = new ObservableBuffer[Snapshot]()
  val inflation = new ObservableBuffer[(Snapshot)]()

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
      snapshots.clear()
    } else {
      val start = (((Inflation, 1.0) :: (AverageSalary, 1.0) :: allocation.allocation(1).toList) map (_._1.startDate)).max
      val end = (((Inflation, 1.0) :: (AverageSalary, 1.0) :: allocation.allocation(1).toList) map (_._1.endDate)).min
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
        case SalaryPercentage => new SalaryPercentage(start, salaryPercentage.value.toDouble / 100.0)
        //case ew FixedUSDAmount(start, 10.0)
      }

      val sim = new Simulator(
        initialAmount,
        allocation,
        instalmentRule,
        strategy)

      val inflationAllocation = new FixedAllocation(Map(Inflation -> 1.0))
      val inflationSim = new Simulator(
        initialAmount,
        inflationAllocation,
        instalmentRule,
        new Split(inflationAllocation))

      val duration = start.until(end, ChronoUnit.MONTHS).toInt + 1

      val results = sim.simulate(start, duration)
      snapshots.setAll(results: _*)
      println(snapshots take 2)

      val stats = new Statistics(initialAmount, results)

      val inflationResults = inflationSim.simulate(start, duration)
      inflation.setAll(inflationResults: _*)

      _minYear.value = snapshots.head.ym.getYear
      _maxYear.value = snapshots.last.ym.getYear
      _summary.value =
        "Last month: " + snapshots.last.ym + "\n" +
        "Last instalment: " + snapshots.last.instalment.formatted("%.2f") + "\n" +
        snapshots.last.portfolio + "\n" +
        "Portfolio Value: " + snapshots.last.value.formatted("%.2f") + "\n" +
        "Total Investment: " + stats.totalInvestment.formatted("%.2f") + "\n" +
        "Total Income: " + stats.totalIncome.formatted("%.2f") + "\n" +
        "Last 12M Income: " + stats.last12MonthsIncome.formatted("%.2f") + "\n" +
        "Inflation-adjusted Return: " +
        ((snapshots.last.value - inflationResults.last.value) / inflationResults.last.value * 100).formatted("%.1f%%") + "\n" +
        "Max absolute drawdown: " +
        (stats.maxAbsoluteDrawdown * 100).formatted("%.1f%%")
    }
  }
}
