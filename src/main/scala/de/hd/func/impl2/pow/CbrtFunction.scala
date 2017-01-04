package de.hd.func.impl2.pow

import de.hd.func.Format
import de.hd.func.impl2.{MathFunction, ScaledByScalar}

/**
  * The cubic root function or 3rd root function:
  * f(x) = cbrt(g(x)) = g(x)&#94;(1/3)
  *
  * @author Henrik Drefs
  */
case class CbrtFunction(override val g: MathFunction) extends AnyFunc2Pow(g, BigDecimal(1) / 3) {

  override def apply(x: BigDecimal): BigDecimal = math.cbrt(g(x).toDouble)

  // todo format adaption
  override def stringify(format: Format): String = s"cbrt(${g.stringify(format)})"
}
