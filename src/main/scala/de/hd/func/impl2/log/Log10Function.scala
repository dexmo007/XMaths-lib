package de.hd.func.impl2.log

import de.hd.func.impl2.MathFunction

/**
  * Created by henri on 12/30/2016.
  */
case class Log10Function(override val f: MathFunction) extends LogBaseFunction(10, f) {

  override def apply(x: BigDecimal): BigDecimal = math.log10(f(x).toDouble)
}
