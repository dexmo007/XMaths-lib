package de.hd.func.impl2.pow

import de.hd.func.Format
import de.hd.func.impl2.{MathFunction, ScaledByScalar}

/**
  * A function that takes an inner function `f` and results in the power to any value `n`:
  * f(x) = g(x)&#94;n
  *
  * @author Henrik Drefs
  */
class AnyFunc2Pow(val g: MathFunction, val n: BigDecimal) extends ScaledByScalar[AnyFunc2Pow] {
  override def apply(x: BigDecimal): BigDecimal = math.pow(x.toDouble, n.toDouble)

  override protected def derive(): MathFunction = ???

  override def antiDerive(c: BigDecimal): MathFunction = ???

  override protected def getConst: Option[BigDecimal] = ???

  override def equalsFunction(that: MathFunction): Boolean = ???

  override def stringify(format: Format): String = format.base(g) + format.pow(n)

}

object AnyFunc2Pow {
  def apply(f: MathFunction, n: BigDecimal) = new AnyFunc2Pow(f, n)

  def unapply(arg: AnyFunc2Pow): Option[(MathFunction, BigDecimal)] = Some((arg.g, arg.n))
}
