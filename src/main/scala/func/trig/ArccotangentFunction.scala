package func.trig

import func.{Function, Polynomial, ScalableFunction}
import func.FuncUtils._

/**
  * Created by Henrik on 7/8/2016.
  */
case class ArccotangentFunction private[func]() extends TrigonometricFunction {

  override def get(x: BigDecimal): BigDecimal = {
    scalar * Math.atan((1.0 / x).toDouble)
  }

  override def derive(): Function = {
    -scalar / Function.sqrt().of(Polynomial(1, 0, 1))
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.linear(scalar) * ArccotangentFunction() + Function.ln(scalar / 2).of(Polynomial(1, 0, 1))
  }

  override def toString: String = scalarString + "acot(x)"
}
