package de.hd.func.impl.trig

import de.hd.func._
import de.hd.func.impl.Polynomial

/**
  * Created by Henrik on 6/27/2016.
  */
case class ArcsineFunction private[func](override val scalar: BigDecimal = 1)
  extends GenTrigonometricFunction[ArcsineFunction](scalar) {

  override val name: String = "asin"

  override def get(x: BigDecimal): BigDecimal = scalar * Math.asin(x.toDouble)

  override def derive(): Function = scalar / Function.sqrt().of(Polynomial(1, 0, -1))

  override def antiderive(c: BigDecimal): Function = Function.linear(scalar) * ArcsineFunction() + Function.sqrt(scalar).of(Polynomial(1, 0, -1))

  override def withScalar(newScalar: BigDecimal): ArcsineFunction = copy(scalar = newScalar)

  override def equals(that: Function): Boolean = ???
}