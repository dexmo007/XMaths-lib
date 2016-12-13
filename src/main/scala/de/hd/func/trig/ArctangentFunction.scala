package de.hd.func.trig

import de.hd.func.{Function, Polynomial}

/**
  * Created by Henrik on 7/8/2016.
  */
case class ArctangentFunction private[func](override val scalar: BigDecimal = 1)
  extends TrigonometricFunction(scalar) {


  override val name: String = "atan"

  override def get(x: BigDecimal): BigDecimal = scalar * Math.atan(x.toDouble)

  override def scaled(scalar: BigDecimal): ArctangentFunction = ArctangentFunction(this.scalar * scalar)

  override def derive(): Function = scalar / Polynomial(1, 0, 1)

  override def antiderive(c: BigDecimal): Function = Function.linear(scalar) * ArctangentFunction() - Function.ln(scalar / 2).of(Polynomial(1, 0, 1)) + c

  override def equals(that: Function): Boolean = ???
}
