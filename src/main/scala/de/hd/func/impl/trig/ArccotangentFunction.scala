package de.hd.func.impl.trig

import de.hd.func.Function
import de.hd.func.impl.Polynomial

/**
  * Created by Henrik on 7/8/2016.
  */
case class ArccotangentFunction private[func](override val scalar: BigDecimal = 1)
  extends GenTrigonometricFunction[ArccotangentFunction](scalar) {

  override val name: String = "acot"

  override def get(x: BigDecimal): BigDecimal = scalar * Math.atan((1.0 / x).toDouble)

  override def derive(): Function = -scalar / Function.sqrt().of(Polynomial(1, 0, 1))

  override def antiderive(c: BigDecimal): Function =
    Function.linear(scalar) * ArccotangentFunction() + Function.ln(scalar / 2).of(Polynomial(1, 0, 1))

  override def withScalar(newScalar: BigDecimal): ArccotangentFunction = copy(scalar = newScalar)

  override def equals(that: Function): Boolean = ???
}
