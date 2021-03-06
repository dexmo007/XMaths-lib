package de.hd.func.impl.trig

import de.hd.func.Function
import de.hd.func.impl.Polynomial

/**
  * Created by Henrik on 7/8/2016.
  */
case class ArccosineFunction private[func](override val scalar: BigDecimal = 1)
  extends GenTrigonometricFunction[ArccosineFunction](scalar) {

  override val name: String = "acos"

  override def get(x: BigDecimal): BigDecimal = scalar * Math.acos(x.toDouble)

  override def derive(): Function = -scalar / Function.sqrt().of(Polynomial(1, 0, -1))

  override def antiderive(c: BigDecimal): Function =
    Function.linear(scalar) * ArccosineFunction() - Function.sqrt(scalar).of(Polynomial(1, 0, -1)) + Function.const(c)

  override def withScalar(newScalar: BigDecimal): ArccosineFunction = copy(scalar = newScalar)

  override def equals(that: Function): Boolean = ???
}
