package func.trig

import func.{Format, Function, Polynomial, ScalableFunction}
import func.FuncUtils._

/**
  * Created by Henrik on 7/8/2016.
  */
case class ArctangentFunction private[func]() extends TrigonometricFunction {

  override val name: String = "atan"

  override def get(x: BigDecimal): BigDecimal = {
    scalar * Math.atan(x.toDouble)
  }

  override def derive(): Function = {
    scalar / Polynomial(1, 0, 1)
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.linear(scalar) * ArctangentFunction() - Function.ln(scalar / 2).of(Polynomial(1, 0, 1)) + c
  }
}
