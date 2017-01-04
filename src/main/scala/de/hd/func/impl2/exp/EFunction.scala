package de.hd.func.impl2.exp

import de.hd.func.impl2.MathFunction

/**
  * Created by henri on 1/2/2017.
  */
case class EFunction(override val f: MathFunction) extends ExponentialFunction(math.E, f) {
  override def apply(x: BigDecimal): BigDecimal = math.exp(x.toDouble)

  override protected def derive(): MathFunction = f.derivative * this

  override def antiDerive(c: BigDecimal): MathFunction =
    if (!f.isLinear) throw new UnsupportedOperationException
    else this / f.gradient
}
