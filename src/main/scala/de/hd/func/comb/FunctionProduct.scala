package de.hd.func.comb

import de.hd.func.trig.TrigonometricFunction
import de.hd.func.{Format, Func2Pow, Function}

/**
  * Created by henri on 12/13/2016.
  */
case class FunctionProduct(private val factors: List[Function]) extends Function {

  override def *(that: Function): Function = that match {
    case trig: TrigonometricFunction =>
      val (matched, rest) = Util.genPartition[TrigonometricFunction](factors, _.getClass == trig.getClass)
      if (matched.size == 1)
      // create func2pow :: rest
      else {
        val (matched, rest) = Util.genPartition[Func2Pow[TrigonometricFunction]](factors, _.inner.getClass == trig.getClass)
        // same again
      }
    case _: Function => super.*(that)
  }

  override def get(x: BigDecimal): BigDecimal = factors.map(_.get(x)).product

  override def scaled(scalar: BigDecimal): Function = FunctionProduct(factors.head.scaled(scalar) :: factors.tail)

  override protected def derive(): Function =
    factors.head.derivative * FunctionProduct(factors.tail) + (factors.head * FunctionProduct(factors.tail).derive())

  override def antiderive(c: BigDecimal): Function = throw new UnsupportedOperationException

  override protected def getConst: Option[BigDecimal] =
    if (factors.exists(_.const.contains(0))) Some(0)
    else if (factors.forall(_.isConst)) Some(factors.map(_.const.get).product)
    else None

  override def equals(that: Function): Boolean = ???

  //todo
  override protected def simplify: Function = super.simplify

  // todo make prettier
  override def stringify(format: Format): String = simplified match {
    case product: FunctionProduct => factors.map(f => s"($f)").mkString("*")
    case any: Function => any.stringify(format)
  }
}
