package de.hd.func.impl2.trig

import de.hd.func.Format
import de.hd.func.impl2.{MathFunction, ScaledByScalar}

/**
  * Superclass of all trigonometric functions, that apply some this function to the value of `f(x)`
  *
  * @author Henrik Drefs
  */
abstract class TrigonometricFunction[T <: TrigonometricFunction[T]](f: MathFunction)
  extends ScaledByScalar[T] {
  this: T =>

  /**
    * mathematical name of this trigonometric function, used in `stringify`
    */
  val name: String

  override def stringify(format: Format): String = s"$name(${f.stringify(format)})"

  override protected def getConst: Option[BigDecimal] = f.const
}
