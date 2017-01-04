package de.hd.func.impl2.log

import de.hd.func.Format
import de.hd.func.impl2.{MathFunction, ScaledByScalar}

/**
  * Created by henri on 12/30/2016.
  */
class LogBaseFunction(val base: BigDecimal, val f: MathFunction) extends ScaledByScalar[LogBaseFunction] {

  require(base > 0 && base != 1, s"Base $base Logarithm is complex or infinity.")

  override def apply(x: BigDecimal): BigDecimal = math.log(f(x).toDouble) / math.log(base.toDouble)

  override protected def derive(): MathFunction = f.derivative * ((f * math.log(base.toDouble)) pow -1)

  override def antiDerive(c: BigDecimal): MathFunction =
    if (!f.isLinear) throw new UnsupportedOperationException
    else (f * MathFunction.ln(f) - f.gradient.x) / (f.gradient * math.log(base.toDouble))

  override protected def getConst: Option[BigDecimal] = f.const.map(c => this (c))

  override def equalsFunction(that: MathFunction): Boolean = that match {
    case LogBaseFunction(this.base, this.f) => true
    case _ => false
  }

  override def stringify(format: Format): String = s"log$base(${f.stringify(format)})"
}

object LogBaseFunction {
  def apply(base: BigDecimal, f: MathFunction) = new LogBaseFunction(base, f)

  def unapply(arg: LogBaseFunction): Option[(BigDecimal, MathFunction)] = Some((arg.base, arg.f))
}
