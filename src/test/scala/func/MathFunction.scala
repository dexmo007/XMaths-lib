package func

/**
  * Created by henri on 12/16/2016.
  */
trait MathFunction {
  def get(x: BigDecimal): BigDecimal

  def *[N: Numeric](n: N): MathFunction
}

object MathFunction {

  trait NumericScalar {
    protected final def scaled[F <: GenFunction[F], N: Numeric](f: F, n: N): F = f.*(n)

    def *[F <: GenFunction[F]](f: F): F
  }

  final implicit class BigDecimalScalar(n: BigDecimal) extends NumericScalar {
    override def *[F <: GenFunction[F]](f: F): F = scaled(f, n)
  }

  final implicit class IntScalar(n: Int) extends NumericScalar {
    override def *[F <: GenFunction[F]](f: F): F = scaled(f, n)
  }

  final implicit class DoubleScalar(n: Double) extends NumericScalar {
    override def *[F <: GenFunction[F]](f: F): F = scaled(f, n)
  }

}
