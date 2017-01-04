package de.hd.func.impl2

import scala.reflect.ClassTag

/**
  * A function that is scaled internally, not with a preceding scalar;
  *
  * that means an instance of this function multiplied by a number returns a new scaled instance of this type
  *
  * @author Henrik Drefs
  */
abstract class SelfScaled[T <: SelfScaled[T] : ClassTag] extends MathFunction {
  this: T =>

  override def *(factor: BigDecimal): T

  def *?(that: T): Option[MathFunction] = None

  override def *(that: MathFunction): MathFunction = that match {
    case t: T => this *? t match {
      case Some(product) => product
      case None => super.*(that)
    }
    case _ => super.*(that)
  }

  /**
    * @param that function to add
    * @return if it is in fact addable resulting in its own type then Some(sum) is returned,
    *         in case of None this + that results in a new instance of the FunctionSum class
    */
  def +?(that: T): Option[T] = None

  override def +(that: MathFunction): MathFunction = that match {
    case t: T => this +? t match {
      case Some(sum) => sum
      case None => super.+(that)
    }
    case _ => super.+(that)
  }
}
