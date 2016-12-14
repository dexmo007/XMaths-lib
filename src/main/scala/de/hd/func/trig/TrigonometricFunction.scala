package de.hd.func.trig

import de.hd.func._

/**
  * Superclass for all trigonometric functions
  */
abstract class TrigonometricFunction(override val scalar: BigDecimal)
  extends ScalarFunction(scalar) {

  /**
    * mathematical name of this trigonometric function, used in `stringify`
    */
  val name: String

  override def +(that: Function): Function = that match {
    case thatTrig: TrigonometricFunction =>
      if (this.getClass == that.getClass)
        this * ((scalar + thatTrig.scalar) / scalar)
      else super.+(that)
    case _ => super.+(that)
  }

  override def stringify(format: Format): String = simplified match {
    case trig: TrigonometricFunction =>
      format.scalar(scalar) + s"$name(x)"
    case f => f.stringify(format)
  }

}
