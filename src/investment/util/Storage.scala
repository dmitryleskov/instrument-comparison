/*
 * Copyright (c) 2016 Dmitry Leskov. All rights reserved.
 */

package investment.util

import investment.SimulationModel.{InstalmentRuleID, StrategyID}
import investment.instruments._

import scala.xml.NodeSeq

/**
  * Created by snowman on 22.12.2016.
  */
object Storage {

  trait Serializer[T] {
    def toXML(t: T): xml.NodeSeq
    def fromXML(x: xml.NodeSeq): Option[T]
  }

  implicit object InstalmentRuleIDSerializer extends Serializer[InstalmentRuleID] {
    def toXML(sid: InstalmentRuleID) = <instalmentRuleID>{sid}</instalmentRuleID>

    def fromXML(x: xml.NodeSeq): Option[InstalmentRuleID] =
      x match {
        case <instalmentRuleID>{sid}</instalmentRuleID> =>
          InstalmentRuleID.values.collectFirst({ case v if sid.text == v.toString => v })
        case _ => None
      }
  }
  implicit object StrategyIDSerializer extends Serializer[StrategyID] {
    def toXML(sid: StrategyID) = <strategyID>{sid}</strategyID>

    def fromXML(x: xml.NodeSeq): Option[StrategyID] =
      x match {
        case <strategyID>{sid}</strategyID> =>
          StrategyID.values.collectFirst({ case v if sid.text == v.toString => v })
        case _ => None
    }
  }

  implicit object InstrumentSerializer extends Serializer[Instrument] {
    def toXML(i: Instrument): xml.Elem =
      i match {
        case s: Stock => <stock>{s.ticker}</stock>
        case DepositRUB => <depositRUB/>
        case Deposit("USD", annualInterest) => <depositUSD>{annualInterest}</depositUSD>
        case Deposit("EUR", annualInterest) => <depositEUR>{annualInterest}</depositEUR>
        case Commodity(ticker) => <commodity>{ticker}</commodity>
        case Cash(currency) => <cash>{currency}</cash>
      }

    def fromXML(x: xml.NodeSeq): Option[Instrument] =
      x match {
        case <stock>{ticker}</stock> => Some(Stock(ticker.text.trim))
        case <depositRUB/> => Some(DepositRUB)
        case <depositUSD>{annualInterest}</depositUSD> => Some(Deposit("USD", annualInterest.text.trim.toDouble))
        case <depositEUR>{annualInterest}</depositEUR> => Some(Deposit("EUR", annualInterest.text.trim.toDouble))
        case <commodity>{ticker}</commodity> => Some(Commodity(ticker.text.trim))
        case <cash>{currency}</cash> => Some(Cash(currency.text.trim))
        case _ => None
      }
  }

  //    implicit def iterableStorage[T](implicit elemStorage: Storage[T]) =
  //      new Storage[Iterable[T]] {

  implicit object AllocationSerializer extends Serializer[List[(Instrument, Int)]] {
    def toXML(allocation: List[(Instrument, Int)]): xml.Elem =
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
