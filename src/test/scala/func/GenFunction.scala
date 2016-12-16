package func

/**
  * Created by henri on 12/16/2016.
  */
trait GenFunction[+T <: MathFunction] extends MathFunction {
  this: T =>

  protected def scaledInternal(scalar: BigDecimal): T

  final override def *[N: Numeric](n: N): T = scaledInternal(BigDecimal(n.toString))
}
