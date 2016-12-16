package de.hd.func.impl.trig

import de.hd.func.Function
import de.hd.func.impl.Polynomial

/**
  * Created by Henrik on 7/8/2016.
  */
case class ArctangentFunction private[func](override val scalar: BigDecimal = 1)
  extends GenTrigonometricFunction[ArctangentFunction](scalar) {

  override val name: String = "atan"

  override def get(x: BigDecimal): BigDecimal = scalar * Math.atan(x.toDouble)

  override def derive(): Function = scalar / Polynomial(1, 0, 1)

  override def antiderive(c: BigDecimal): Function = Function.linear(scalar) * ArctangentFunction() - Function.ln(scalar / 2).of(Polynomial(1, 0, 1)) + c

  override def withScalar(newScalar: BigDecimal): ArctangentFunction = copy(scalar = newScalar)

  override def equals(that: Function): Boolean = ???
}
