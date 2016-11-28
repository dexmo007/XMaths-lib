package func.log

import func.{Function, ScalableFunction}
import func.FuncUtils.MathString

/**
  * Created by Henrik on 6/25/2016.
  */
case class LnFunction private[func]() extends ScalableFunction {

  override def get(x: BigDecimal): BigDecimal = {
    require(x > 0, "ln(" + x + ") is undefined!")
    scalar * Math.log(x.toDouble)
  }

  override def derive(): Function = {
    Function.const(scalar) / Function.linear(1)
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.linear(scalar) * (LnFunction() - 1) + c
  }

  override def toString: String = scalar.toScalarString + "ln(x)"
}
