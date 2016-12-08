package func.trig

import func.{Format, Function, Polynomial, ScalableFunction}
import func.FuncUtils._

/**
  * Created by Henrik on 6/27/2016.
  */
case class ArcsineFunction private[func]() extends TrigonometricFunction {

  override val name: String = "asin"

  override def get(x: BigDecimal): BigDecimal = {
    scalar * Math.asin(x.toDouble)
  }

  override def derive(): Function = {
    scalar / Function.sqrt().of(Polynomial(1, 0, -1))
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.linear(scalar) * ArcsineFunction() + Function.sqrt(scalar).of(Polynomial(1, 0, -1))
  }

  override def stringify(format: Format): String = format.scalar(scalar) + "asin(x)"

  override def equals(that: Function): Boolean = ???
}