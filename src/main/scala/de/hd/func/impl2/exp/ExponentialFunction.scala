package de.hd.func.impl2.exp

import de.hd.func.Format
import de.hd.func.impl2.log.LogBaseFunction
import de.hd.func.impl2.{MathFunction, ScaledByScalar}

/**
  * Created by henri on 1/2/2017.
  */
class ExponentialFunction(val base: BigDecimal, val f: MathFunction) extends ScaledByScalar[ExponentialFunction] {

  override def apply(x: BigDecimal): BigDecimal = {
    val fx = f(x)
    if (fx.isValidInt) base pow fx.toInt
    else base pow fx
  }

  override protected def derive(): MathFunction = this * math.log(base.toDouble) * f.derivative

  override def antiDerive(c: BigDecimal): MathFunction =
    if (!f.isLinear) throw new UnsupportedOperationException
    else this / (f.gradient * math.log(base.toDouble))

  override def *(that: MathFunction): MathFunction = that match {
    case ExponentialFunction(this.base, thatF) => ExponentialFunction(base, this.f * thatF)
    case _ => super.*(that)
  }

  override def pow(n: Int): MathFunction = ExponentialFunction(base, this.f * n)

  override def pow(n: BigDecimal): MathFunction = ExponentialFunction(base, this.f * n)

  override protected def getConst: Option[BigDecimal] =
    if (base == 0 || base == 1) Some(base)
    else f.const.map(c => this (c))

  override protected def simplify: MathFunction = f match {
    case LogBaseFunction(this.base, thatF) => thatF
    case _ => this
  }

  override def equalsFunction(that: MathFunction): Boolean = that match {
    case ExponentialFunction(this.base, this.f) => true
    case _ => false
  }

  override def stringify(format: Format): String = format.base(base) + format.pow(f)
}

object ExponentialFunction {
  def apply(base: BigDecimal, f: MathFunction) = new ExponentialFunction(base, f)

  def unapply(arg: ExponentialFunction): Option[(BigDecimal, MathFunction)] = Some((arg.base, arg.f))
}
