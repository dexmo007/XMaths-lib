package de.hd.func.impl.trig

import de.hd.func._
import de.hd.func.impl.MethodFunction
import de.hd.func.impl.log.LnFunction

/**
  * Created by Henrik on 6/25/2016.
  */
case class TangentFunction private[func](override val scalar: BigDecimal = 1) extends GenTrigonometricFunction[TangentFunction](scalar) {

  override val name: String = "tan"

  override def get(x: BigDecimal): BigDecimal = scalar * Math.tan(x.toDouble)

  override protected def derive(): Function = scalar / Function.cos().pow(2)

  override def antiderive(c: BigDecimal): Function = (LnFunction() * scalar) of MethodFunction(x => Math.abs(Math.cos(x.toDouble)))

  override def withScalar(newScalar: BigDecimal): TangentFunction = copy(scalar = newScalar)

  override def equals(that: Function): Boolean = ???
}
