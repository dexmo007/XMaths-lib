package func.trig

import func.FuncUtils.MathString
import func.{Function, ScalableFunction}

/**
  * Created by Henrik on 6/25/2016.
  */
case class CosineFunction private[func]() extends ScalableFunction {

  override def get(x: BigDecimal): BigDecimal = {
    scalar * Math.cos(x.toDouble)
  }

  override def derive(): Function = {
    Function.sin(-scalar)
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.sin(scalar) + c
  }

  override def toString: String = {
    scalar.toScalarString + "cos(x)"
  }
}
