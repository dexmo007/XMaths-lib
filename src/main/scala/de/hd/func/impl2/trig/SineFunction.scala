package de.hd.func.impl2.trig

import de.hd.func.impl2.MathFunction

/**
  * A sine function
  *
  * @author Henrik Drefs
  */
case class SineFunction(f: MathFunction) extends TrigonometricFunction[SineFunction](f) {

  override val name: String = "sin"

  override def apply(x: BigDecimal): BigDecimal = math.sin(f(x).toDouble)

  override protected def derive(): MathFunction = f.derivative * MathFunction.cos(f)

  override def antiDerive(c: BigDecimal): MathFunction =
    if (!f.isLinear) throw new UnsupportedOperationException
    else MathFunction.cos(f) / f.gradient

  override def equalsFunction(that: MathFunction): Boolean = that match {
    case SineFunction(f2) => f == f2
    // todo other equality
    case _ => false
  }
}
