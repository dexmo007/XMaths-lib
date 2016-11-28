package func.trig

import func.{Function, MethodFunction, ScalableFunction}
import func.log.LnFunction

/**
  * Created by Henrik on 6/25/2016.
  */
case class TangentFunction private[func]() extends ScalableFunction {

  override def get(x: BigDecimal): BigDecimal = {
    scalar * Math.tan(x.toDouble)
  }

  override def derive(): Function = {
    scalar / Function.cos().pow(2)
  }

  override def antiderive(c: BigDecimal): Function = {
    LnFunction(scalar).of(MethodFunction(x => Math.abs(Math.cos(x.toDouble))))
  }
}
