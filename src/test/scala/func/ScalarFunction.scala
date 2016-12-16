package func

/**
  * Created by henri on 12/16/2016.
  */
trait ScalarFunction {
  def withScalar(newScalar: BigDecimal): ScalarFunction

  def withAddedScalar(toAdd: BigDecimal): ScalarFunction

}

abstract class GenScalarFunction[+T <: ScalarFunction] extends ScalarFunction {
  this: T =>
  override def withScalar(newScalar: BigDecimal): T

  override def withAddedScalar(toAdd: BigDecimal): T
}
