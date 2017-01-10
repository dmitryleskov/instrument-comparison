/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment

import java.time.YearMonth

import investment.SimulationModel.InstalmentRuleID.{AnnualIncrease, FixedAmount, InflationAdjusted, SalaryPercentage}
import investment.SimulationModel.StrategyID._
import investment.data.{AverageSalary, Inflation}
import investment.instruments.Instrument

import scalafx.beans.property._

object SimulationModel {
  private val _minYear = ReadOnlyIntegerWrapper(Instrument.startDate.getYear)
  val minYear: ReadOnlyIntegerProperty = _minYear.readOnlyProperty
  private val _maxYear = ReadOnlyIntegerWrapper(YearMonth.now.getYear)
  val maxYear: ReadOnlyIntegerProperty = _maxYear.readOnlyProperty
  private val _statistics = ReadOnlyObjectWrapper[Statistics](null: Statistics)
  val statistics: ReadOnlyObjectProperty[Statistics] = _statistics.readOnlyProperty

  sealed abstract class StrategyID
  object StrategyID {
    case object Split extends StrategyID {override def toString() = "Simple Split"}
    case object BalanceGradually extends StrategyID {override def toString() = "Balance Gradually"}
    case object RebalanceMonthly extends StrategyID {override def toString() = "Rebalance Monthly"}
    case object RebalanceQuarterly extends StrategyID {override def toString() = "Rebalance Quarterly"}
    case object RebalanceAnnually extends StrategyID {override def toString() = "Rebalance Annually"}
    val values = Seq(Split, BalanceGradually, RebalanceMonthly, RebalanceQuarterly, RebalanceAnnually)
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

  val minStartDate = ObjectProperty[YearMonth](this, "minStartDate", Instrument.startDate)
  minStartDate.onChange(updateResults)

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
      val strategy = strategyId.value match {
        case Split => new Split(allocation)
        case BalanceGradually => new BalanceGradually(allocation)
        case RebalanceMonthly => new RebalanceMonthly(allocation)
        case RebalanceQuarterly => new RebalanceQuarterly(allocation)
        case RebalanceAnnually => new RebalanceAnnually(allocation)
      }
      val instalmentRule = instalmentRuleId.value match {
        case FixedAmount => new FixedAmount(initialInstalment.value)
        case AnnualIncrease => new AnnualIncrease(initialInstalment.value, 1.05)
        case InflationAdjusted => new InflationAdjusted(initialInstalment.value)
        case SalaryPercentage => new SalaryPercentage(salaryPercentage.value.toDouble / 100.0)
        //case ew FixedUSDAmount(start, 10.0)
      }
      if (initialAmount != 0 || instalmentRule.instalment(minStartDate.value, 1) != 0) {
        val simulator = new Simulator(
          initialAmount,
          allocation,
          instalmentRule,
          strategy)
        val stats = new Statistics(minStartDate.value, simulator)
        _statistics.value = stats
        _minYear.value = stats.start.getYear
        _maxYear.value = stats.end.getYear
      } else
        _statistics.value = null
    }
  }
}
