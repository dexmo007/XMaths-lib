package de.hd.func.impl.trig

import de.hd.func._

/**
  * Superclass for all trigonometric functions
  */
trait TrigonometricFunction extends ScalarFunction {
  /**
    * mathematical name of this trigonometric function, used in `stringify`
    */
  val name: String
}

abstract class GenTrigonometricFunction[+T <: TrigonometricFunction](override val scalar: BigDecimal)
  extends GenScalarFunction[T](scalar) with TrigonometricFunction {
  this: T =>

  override def +(that: Function): Function = that match {
    case thatTrig: TrigonometricFunction =>
      if (this.getClass == that.getClass)
        this * ((scalar + thatTrig.scalar) / scalar)
      else super.+(that)
    case _ => super.+(that)
  }

  override def stringify(format: Format): String = simplified match {
    case trig: TrigonometricFunction =>
      format.scalar(trig.scalar) + s"${trig.name}(x)"
    case f => f.stringify(format)
  }

}
