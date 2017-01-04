package de.hd.func.impl.comb

import de.hd.func.{Format, Function, GenFunction}

/**
  * Created by henri on 12/14/2016.
  */
case class FunctionQuotient(dividend: Function, divisor: Function) extends GenFunction[FunctionQuotient] {

  if (divisor.isConst(0))
    throw new ArithmeticException("division by 0 undefined")

  override def get(x: BigDecimal): BigDecimal = dividend.get(x) / divisor.get(x)

  override def scaledInternal(scalar: BigDecimal): FunctionQuotient = FunctionQuotient(dividend * scalar, divisor)

  override protected def derive(): Function =
    (dividend.derivative * divisor - (dividend * divisor.derivative)) / divisor.pow(2)

  // todo work out cases
  override def antiderive(c: BigDecimal): Function = throw new UnsupportedOperationException

  override protected def getConst: Option[BigDecimal] =
    if (dividend.isConst(0)) Some(0)
    else if (dividend.isConst && divisor.isConst) Some(dividend.const.get / dividend.const.get)
    else None

  override protected def simplify: Function = {
    if (isConst) Function.const(const.get)
    else if (dividend.isConst) divisor.pow(-1) * dividend.const.get
    else if (divisor.isConst) dividend / divisor.const.get
    else this
  }

  override def equals(that: Function): Boolean = ???

  override def stringify(format: Format): String = simplified match {
    case quot: FunctionQuotient =>
      s"${format.fraction(quot.dividend.stringify(format), quot.divisor.stringify(format))}"
    case f: Function => f.stringify(format)
  }
}
