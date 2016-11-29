package func.trig

import func.{Function, Polynomial, ScalableFunction}
import func.FuncUtils._

/**
  * Created by Henrik on 7/8/2016.
  */
case class ArccosineFunction private[func]() extends TrigonometricFunction {

  override def get(x: BigDecimal): BigDecimal = {
    scalar * Math.acos(x.toDouble)
  }

  override def derive(): Function = {
    -scalar / Function.sqrt().of(Polynomial(1, 0, -1))
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.linear(scalar) * ArccosineFunction() - Function.sqrt(scalar).of(Polynomial(1, 0, -1)) + c
  }

  override def toString: String = scalarString + "acos(x)"
}
