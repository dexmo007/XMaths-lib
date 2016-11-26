package func.log

import func.{Function, FunctionException}

/**
  * Created by Henrik on 6/25/2016.
  */
case class LogBaseFunction private[func](base: Int, scale: BigDecimal) extends Function {

  require(base > 0 && base != 1, "Base must not be " + base)

  var scl: BigDecimal = scale

  override def get(x: BigDecimal): BigDecimal = {
    require(x > 0, "log" + base + "(" + x + ") is undefined!")
    scl * Math.log(x.toDouble) / Math.log(base)
  }

  override def derive(): Function = {
    Function.const(scl) / Function.linear(Math.log(base))
  }

  override def scale(factor: BigDecimal): Unit = {
    scl *= factor
  }

  override def antiderive(c: BigDecimal): Function = ???
}
