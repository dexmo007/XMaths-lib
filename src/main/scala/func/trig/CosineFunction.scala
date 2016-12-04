package func.trig

import func.{Format, Function, ScalableFunction}
import func.FuncUtils._

/**
  * Created by Henrik on 6/25/2016.
  */
case class CosineFunction private[func]() extends TrigonometricFunction {

  override val name: String = "cos"

  override def get(x: BigDecimal): BigDecimal = {
    scalar * Math.cos(x.toDouble)
  }

  override def derive(): Function = {
    Function.sin(-scalar)
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.sin(scalar) + c
  }
}
