package func.trig

import func.{Function, Polynomial, ScalableFunction}
import func.FuncUtils._

/**
  * Created by Henrik on 6/27/2016.
  */
case class ArcsineFunction private[func]() extends TrigonometricFunction {

  override def get(x: BigDecimal): BigDecimal = {
    scalar * Math.asin(x.toDouble)
  }

  override def derive(): Function = {
    scalar / Function.sqrt().of(Polynomial(1, 0, -1))
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.linear(scalar) * ArcsineFunction() + Function.sqrt(scalar).of(Polynomial(1, 0, -1))
  }

  override def toString: String = scalarString + "asin(x)"
}
