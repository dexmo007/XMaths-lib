package de.hd.func.impl2

import de.hd.func.Format

import scala.language.postfixOps

/**
  * A polynomial that is represented by a map of exponents to coefficients
  *
  * @author Henrik Drefs
  */
case class Polynomial(private val terms: Map[Int, BigDecimal]) extends SelfScaled[Polynomial] {

  require(terms.keys.forall(_ >= 0), "A polynomial must only have positive exponents.")

  override def apply(x: BigDecimal): BigDecimal = terms.map {
    case (exp, coeff) => coeff * x.pow(exp)
  }.sum

  override def *(factor: BigDecimal): Polynomial = Polynomial(terms mapValues (_ * factor))

  override def *?(that: Polynomial): Option[Polynomial] =
    Some(
      if (that.isConst) this * that.const.get
      else if (this.isConst) that * this.const.get
      else {
        val product = (for {
          (exp, coeff) <- this.simplified.terms.toList
          (thatExp, thatCoeff) <- that.simplified.terms
        } yield (exp + thatExp, coeff * thatCoeff)).foldLeft(Map.empty[Int, BigDecimal])({
          case (rest, (exp, coeff)) => rest + (exp -> (coeff + rest.getOrElse(exp, 0)))
        })
        Polynomial(product)
      })

  override def +?(that: Polynomial): Option[Polynomial] = Some(
    Polynomial((that.terms foldLeft this.terms) ((ts, t) => {
      val (exp, coeff) = t
      ts + (exp -> (coeff + ts.getOrElse(exp, 0)))
    }))
  )

  override lazy val derivative: Polynomial = derive()

  override protected def derive(): Polynomial = {
    val derived = terms.filterNot {
      case (exp, coeff) => exp == 0 || coeff == 0
    } map { case (exp, coeff) => (exp - 1, coeff * exp) }
    Polynomial(derived)
  }

  override lazy val antiDerivative: Polynomial = antiDerive()

  override def antiDerive(c: BigDecimal): Polynomial = {
    val antiDerived = simplified.terms map {
      case (exp, coeff) => (exp + 1, coeff / (exp + 1))
    }
    Polynomial(antiDerived + (0 -> c))
  }

  lazy val level: Int = terms.keys.max

  override lazy val isLinear: Boolean = level < 2

  override protected def getConst: Option[BigDecimal] =
    if (level == 0) Some(terms.getOrElse(0, 0))
    else None

  override lazy val simplified: Polynomial = simplify

  override protected def simplify: Polynomial = Polynomial(terms.filterNot { case (_, coeff) => coeff == 0 })

  override def equalsFunction(that: MathFunction): Boolean = that match {
    // todo check if map equals always does the job correctly?
    case Polynomial(thatTerms) => terms == thatTerms
    case _ => false
  }

  override def stringify(format: Format): String = {
    if (isConst)
      return format.num(const.get)
    val sorted = simplified.terms.toList.sortWith(_._1 > _._1)
    val (firstExp, firstCoeff) = sorted.head

    def stringFor(exp: Int, coeff: BigDecimal): String =
      if (exp == 0) format.num(coeff)
      else format.scalar(coeff) + "x" + format.pow(exp)

    stringFor(firstExp, firstCoeff) :: (
      for ((exp, coeff) <- sorted.tail) yield {
        if (coeff > 0) "+" + stringFor(exp, coeff)
        else stringFor(exp, coeff)
      }) mkString
  }
}

object Polynomial {

  def apply(c: BigDecimal) = new Polynomial(Map(0 -> c))

  def apply(terms: (Int, BigDecimal)*) = new Polynomial(terms.toMap)

}
