package de.hd.func.trig

import de.hd.func.Function

/**
  * Created by Henrik on 6/25/2016.
  */
case class CosineFunction private[func](override val scalar: BigDecimal = 1)
  extends TrigonometricFunction(scalar) {


  override val name: String = "cos"

  override def get(x: BigDecimal): BigDecimal = scalar * Math.cos(x.toDouble)

  override def scaled(scalar: BigDecimal): CosineFunction = CosineFunction(this.scalar * scalar)

  override def derive(): Function = Function.sin(-scalar)

  override def antiderive(c: BigDecimal): Function = Function.sin(scalar) + c

  override def equals(that: Function): Boolean = ???
}
