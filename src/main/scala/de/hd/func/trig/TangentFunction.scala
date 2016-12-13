package de.hd.func.trig

import de.hd.func._
import de.hd.func.log.LnFunction

/**
  * Created by Henrik on 6/25/2016.
  */
case class TangentFunction private[func](override val scalar: BigDecimal = 1) extends TrigonometricFunction(scalar) {

  override val name: String = "tan"

  override def get(x: BigDecimal): BigDecimal = scalar * Math.tan(x.toDouble)

  override def scaled(scalar: BigDecimal): TangentFunction = copy(scalar = this.scalar * scalar)

  override protected def derive(): Function = scalar / Function.cos().pow(2)

  override def antiderive(c: BigDecimal): Function = LnFunction() * scalar of MethodFunction(x => Math.abs(Math.cos(x.toDouble)))

  override def equals(that: Function): Boolean = ???

}
