package func

import org.apache.commons.math3.fraction.Fraction

import scala.language.implicitConversions

/**
  * Created by henri on 12/17/2016.
  */
object BigDecimalLikePredef {

  /**
    * Type-class for a type that can be converted to BigDecimal
    *
    * @tparam T a numeric type
    */
  trait BigDecimalLike[T] {
    def get(t: T): BigDecimal
  }

  implicit object BigDecimalLikeBigDecimal extends BigDecimalLike[BigDecimal] {
    override def get(t: BigDecimal): BigDecimal = t
  }

  implicit object BigDecimalLikeInt extends BigDecimalLike[Int] {
    override def get(t: Int): BigDecimal = BigDecimal(t)
  }

  implicit object BigDecimalLikeDouble extends BigDecimalLike[Double] {
    override def get(t: Double): BigDecimal = BigDecimal(t)
  }

  implicit object BigDecimalLikeFloat extends BigDecimalLike[Float] {
    override def get(t: Float): BigDecimal = BigDecimal(t)
  }

  implicit object BigDecimalLikeBigInt extends BigDecimalLike[BigInt] {
    override def get(t: BigInt): BigDecimal = BigDecimal(t)
  }

  implicit object BigDecimalLikeShort extends BigDecimalLike[Short] {
    override def get(t: Short): BigDecimal = BigDecimal(t)
  }

  implicit object BigDecimalLikeChar extends BigDecimalLike[Char] {
    override def get(t: Char): BigDecimal = BigDecimal(t)
  }

  implicit object BigDecimalLikeFraction extends BigDecimalLike[Fraction] {
    override def get(t: Fraction): BigDecimal = BigDecimal(t.getNumerator) / BigDecimal(t.getDenominator)
  }

}
