package de.hd.func.impl2.pow

import de.hd.func.Format
import de.hd.func.impl2.{MathFunction, ScaledByScalar}

import scala.language.implicitConversions

/**
  * A function that takes an inner function `f` and results in the power to any value `n`:
  * f(x) = g(x)&#94;n
  *
  * @author Henrik Drefs
  */
class AnyFunc2Pow(val g: MathFunction, val n: BigDecimal) extends ScaledByScalar[AnyFunc2Pow] {

  override def apply(x: BigDecimal): BigDecimal = g(x) pow n

  override protected def derive(): MathFunction = g.derivative * ((g pow (n - 1)) * n)

  override def antiDerive(c: BigDecimal): MathFunction =
    if (!g.isLinear) throw new UnsupportedOperationException
    else if (n == -1) MathFunction.ln(g) / g.gradient
    else g.derivative * (g pow (n + 1)) / ((n + 1) * g.gradient) + c

  override def *(that: MathFunction): MathFunction = that match {
    case AnyFunc2Pow(this.g, thatN) => AnyFunc2Pow(g, this.n + thatN)
    case _ =>
      if (that == this.g) AnyFunc2Pow(g, n + 1)
      else super.*(that)
  }

  override def pow(n: Int): MathFunction = AnyFunc2Pow(g, this.n * n)

  override def pow(n: BigDecimal): MathFunction = AnyFunc2Pow(g, this.n * n)

  override protected def getConst: Option[BigDecimal] =
    if (n == 0) Some(1)
    else if (g.isConst) g.const.map(_ pow n)
    else None

  override protected def simplify: MathFunction =
    if (isConst) const.get
    else if (n == 1) g.simplified
    else g.simplified match {
      case AnyFunc2Pow(g.simplified, n2) =>
        if (n == 1 / n2) g.simplified
        else AnyFunc2Pow(g.simplified, n)
      case _ => AnyFunc2Pow(g.simplified, n)
    }

  override def equalsFunction(that: MathFunction): Boolean = that match {
    case AnyFunc2Pow(g2, n2) => g == g2 && n == n2
    case _ => false
  }

  override def stringify(format: Format): String = format.base(g) + format.pow(n)


}

object AnyFunc2Pow {
  def apply(f: MathFunction, n: BigDecimal) = new AnyFunc2Pow(f, n)

  def unapply(arg: AnyFunc2Pow): Option[(MathFunction, BigDecimal)] = Some((arg.g, arg.n))
}
