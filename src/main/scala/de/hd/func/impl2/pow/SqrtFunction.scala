package de.hd.func.impl2.pow

import de.hd.func.Format
import de.hd.func.impl2.{MathFunction, ScaledByScalar}

/**
  * The square root function or 2nd root function:
  * f(x) = sqrt(g(x)) = g(x)&#94;(1/2)
  *
  * @author Henrik Drefs
  */
case class SqrtFunction(override val g: MathFunction) extends AnyFunc2Pow(g, BigDecimal(1) / 2) {

  override def apply(x: BigDecimal): BigDecimal = math.sqrt(g(x).toDouble)

  // todo adapt format so it can display sqrt and cbrt
  override def stringify(format: Format): String = s"sqrt(${g.stringify(format)})"
}
