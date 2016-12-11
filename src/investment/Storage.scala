package investment

import scala.xml.{Node, NodeSeq}

object Storage {

  trait Serializer[T] {
    def toXML(t: T): xml.NodeSeq
    def fromXML(x: xml.NodeSeq): Option[T]
  }

  implicit object InstrumentSerializer extends Serializer[Instrument] {
    def toXML(i: Instrument): xml.Elem =
      i match {
        case s: Stock => <stock>{s.ticker}</stock>
        case CashEUR => <cashEUR/>
        case CashUSD => <cashUSD/>
        case _ => <хрень/>
      }

    def fromXML(x: xml.NodeSeq): Option[Instrument] =
      x match {
        case <stock>{ticker}</stock> => Some(Stock(ticker.text.trim))
        case <cashEUR/> => Some(CashEUR)
        case <cashUSD/> => Some(CashUSD)
        case _ => None
      }
  }

//    implicit def iterableStorage[T](implicit elemStorage: Storage[T]) =
//      new Storage[Iterable[T]] {

  implicit object PortfolioSerializer extends Serializer[List[(Instrument, Int)]] {
    def toXML(portfolio: List[(Instrument, Int)]): xml.Elem =
      <portfolio>
        {for ((instrument, weight) <- portfolio)
        yield <position>
          <instrument>{Storage.toXML(instrument)}</instrument>
          <weight>{weight}</weight>
        </position>}
      </portfolio>


    def fromXML(portfolioXML: NodeSeq): Option[List[(Instrument, Int)]] = {
      portfolioXML match {
        case <portfolio>{positions @ _*}</portfolio> =>
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