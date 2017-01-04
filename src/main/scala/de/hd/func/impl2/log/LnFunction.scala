package de.hd.func.impl2.log

import de.hd.func.Format
import de.hd.func.impl2.MathFunction

/**
  * Created by henri on 12/30/2016.
  */
case class LnFunction(override val f: MathFunction) extends LogBaseFunction(math.E, f) {

  override def apply(x: BigDecimal): BigDecimal = math.log(f(x).toDouble)

  override protected def derive(): MathFunction = f.derivative / f

  override def antiDerive(c: BigDecimal): MathFunction =
    if (!f.isLinear) throw new UnsupportedOperationException
    else (f * this - f.gradient.x) / f.gradient

  override def stringify(format: Format): String = s"ln(${f.stringify(format)})"
}
