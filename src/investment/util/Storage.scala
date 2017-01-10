/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment.util

import investment.AllocationModel.DateRange
import investment.SimulationModel.{InstalmentRuleID, StrategyID}
import investment.instruments.{Deposit, _}

import scala.xml.{Elem, NodeSeq}

object Storage {

  trait Serializer[T] {
    def toXML(t: T): xml.NodeSeq
    def fromXML(x: xml.NodeSeq): Option[T]
  }

  implicit object DateRangeSerializer extends Serializer[DateRange] {
    override def toXML(r: DateRange): NodeSeq = <dateRange>{r}</dateRange>

    override def fromXML(x: NodeSeq): Option[DateRange] =
      x match {
        case <dateRange>{r}</dateRange> =>
          DateRange.values.collectFirst({ case v if r.text == v.toString => v })
        case _ => None
      }
  }

  implicit object InstalmentRuleIDSerializer extends Serializer[InstalmentRuleID] {
    def toXML(sid: InstalmentRuleID): Elem = <instalmentRuleID>{sid}</instalmentRuleID>

    def fromXML(x: xml.NodeSeq): Option[InstalmentRuleID] =
      x match {
        case <instalmentRuleID>{sid}</instalmentRuleID> =>
          InstalmentRuleID.values.collectFirst({ case v if sid.text == v.toString => v })
        case _ => None
      }
  }
  implicit object StrategyIDSerializer extends Serializer[StrategyID] {
    def toXML(sid: StrategyID): Elem = <strategyID>{sid}</strategyID>

    def fromXML(x: xml.NodeSeq): Option[StrategyID] =
      x match {
        case <strategyID>{sid}</strategyID> =>
          StrategyID.values.collectFirst({ case v if sid.text == v.toString => v })
        case _ => None
    }
  }

  implicit object InstrumentSerializer extends Serializer[Instrument] {
    def toXML(i: Instrument): Elem =
      i match {
        case s: Stock => <stock>{s.ticker}</stock>
        case Deposit(currency) => <deposit>{currency}</deposit>
        case Commodity(ticker) => <commodity>{ticker}</commodity>
        case Cash(currency) => <cash>{currency}</cash>
      }

    def fromXML(x: xml.NodeSeq): Option[Instrument] =
      x match {
        case <stock>{ticker}</stock> => Some(Stock(ticker.text.trim))
        case <deposit>{currency}</deposit> => Some(Deposit(currency.text.trim))
        case <commodity>{ticker}</commodity> => Some(Commodity(ticker.text.trim))
        case <cash>{currency}</cash> => Some(Cash(currency.text.trim))
        case _ => None
      }
  }

  implicit object AllocationSerializer extends Serializer[List[(Instrument, Int)]] {
    def toXML(allocation: List[(Instrument, Int)]): Elem =
      <allocation>
        {for ((instrument, weight) <- allocation)
        yield <position>
          <instrument>{Storage.toXML(instrument)}</instrument>
          <weight>{weight}</weight>
        </position>}
      </allocation>

    def fromXML(allocationXML: NodeSeq): Option[List[(Instrument, Int)]] = {
      allocationXML match {
        case <allocation>{positions @ _*}</allocation> =>
          Some((for { position @ <position>{_*}</position> <- positions
                      i <- Storage.fromXML[Instrument]((position \ "instrument" \ "_").head)
                      w = (position \ "weight").text.trim.toInt }
            yield (i, w)).toList)
        case _ => None
      }
    }

  }

  def toXML[T](instance: T)(implicit serializer: Serializer[T]) =
    serializer.toXML(instance)

  def fromXML[T](x: xml.NodeSeq)(implicit serializer: Serializer[T]) =
    serializer.fromXML(x)
}
