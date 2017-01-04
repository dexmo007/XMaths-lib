package de.hd.func.impl2.trig

import de.hd.func.impl2.MathFunction

/**
  * Created by henri on 12/29/2016.
  */
case class CosineFunction(f: MathFunction) extends TrigonometricFunction[CosineFunction](f) {

  override val name: String = "cos"

  override def apply(x: BigDecimal): BigDecimal = math.cos(f(x).toDouble)

  override protected def derive(): MathFunction = -MathFunction.sin(f)

  override def antiDerive(c: BigDecimal): MathFunction = ???

  override def equalsFunction(that: MathFunction): Boolean = ???
}
