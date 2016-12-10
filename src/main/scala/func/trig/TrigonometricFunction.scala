package func.trig

import func.{Format, Function, GenCloneable, ScalableFunction}

/**
  * Superclass for all trigonometric functions
  */
abstract class TrigonometricFunction extends ScalableFunction with GenCloneable[TrigonometricFunction] {
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

  override def stringify(format: Format): String = format.scalar(scalar) + s"$name(x)"

}
