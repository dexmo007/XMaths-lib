package de.hd.func.impl.trig

import de.hd.func.Function

/**
  * Created by Henrik on 6/25/2016.
  */
case class CosineFunction private[func](override val scalar: BigDecimal = 1)
  extends GenTrigonometricFunction[CosineFunction](scalar) {

  override val name: String = "cos"

  override def get(x: BigDecimal): BigDecimal = scalar * Math.cos(x.toDouble)

  override def derive(): Function = Function.sin(-scalar)

  override def antiderive(c: BigDecimal): Function = Function.sin(scalar) + Function.const(c)

  override def equals(that: Function): Boolean = ???

  override def withScalar(newScalar: BigDecimal): CosineFunction = copy(scalar = newScalar)
}
