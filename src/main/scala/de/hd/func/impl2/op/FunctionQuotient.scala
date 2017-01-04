package de.hd.func.impl2.op

import de.hd.func.Format
import de.hd.func.impl2.{MathFunction, SelfScaled}

/**
  * A quotient of two functions
  *
  * @author Henrik Drefs
  */
case class FunctionQuotient(dividend: MathFunction, divisor: MathFunction) extends SelfScaled[FunctionQuotient] {

  override def apply(x: BigDecimal): BigDecimal = dividend(x) / divisor(x)

  override def *(factor: BigDecimal): FunctionQuotient = copy(dividend = dividend * factor)

  override def +?(that: FunctionQuotient): Option[FunctionQuotient] =
    Some(FunctionQuotient(this.dividend * that.dividend, this.divisor * that.divisor))

  override protected def derive(): MathFunction =
    (dividend.derivative * divisor - (dividend * divisor.derivative)) / divisor.pow(2)

  //todo work out cases
  override def antiDerive(c: BigDecimal): MathFunction = ???

  override protected def getConst: Option[BigDecimal] =
    if (dividend.isConst(0) && !divisor.isConst(0)) Some(0)
    else if (dividend.isConst && divisor.isConst) Some(dividend.const.get / divisor.const.get)
    else None

  override protected def simplify: MathFunction = {
    if (isConst) const.get
    else if (dividend.isConst) divisor.pow(-1) * dividend.const.get
    else if (divisor.isConst) dividend / divisor.const.get
    else FunctionQuotient(dividend.simplified, divisor.simplified)
  }

  override def equalsFunction(that: MathFunction): Boolean = that match {
    case FunctionQuotient(dividend2, divisor2) => dividend == dividend2 && divisor == divisor2
    case _ => simplified match {
      case FunctionQuotient(_, _) => false
      case _ => simplified == that
    }
  }

  override def stringify(format: Format): String =
    s"${format.fraction(dividend.stringify(format), divisor.stringify(format))}"
}
