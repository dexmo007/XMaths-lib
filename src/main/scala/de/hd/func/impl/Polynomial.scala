package de.hd.func.impl

import de.hd.func.{Format, Function, GenFunction}

/**
  * Polynomial function saving the scalars in a list, index represents the power
  *
  */
class Polynomial(val scalars: List[BigDecimal]) extends GenFunction[Polynomial] {

  def get(x: BigDecimal): BigDecimal = {
    var res: BigDecimal = 0
    for (k <- 0 to level) {
      res += scalars(k) * x.pow(k)
    }
    res
  }

  override def scaledInternal(scalar: BigDecimal): Polynomial = Polynomial(this.scalars.map(_ * scalar))

  override def derive(): Function = {
    val buf = scalars.toBuffer
    for (i <- 2 to level) {
      buf(i) *= i
    }
    buf.remove(0)
    Polynomial(buf.toList)
  }

  override def antiderive(c: BigDecimal): Function = {
    val buf = scalars.toBuffer
    for (i <- 0 to level) {
      buf(i) /= (i + 1)
    }
    buf.insert(0, c)
    Polynomial(buf.toList)
  }

  override def getConst: Option[BigDecimal] = {
    if (level == 0 || scalars.drop(1).forall(n => n == 0))
      Some(scalars.head)
    else None
  }

  override lazy val isLinear: Boolean = isConst || level < 2 || scalars.drop(2).forall(_ == 0)

  lazy val level: Int = {
    def calcLevel: Int = {
      for (i <- scalars.indices.reverse)
        if (scalars(i) != 0)
          return i
      0
    }

    calcLevel
  }

  def getFirstEffectiveScale: BigDecimal = {
    for (scale <- scalars) {
      if (scale != 0) {
        return scale
      }
    }
    0
  }

  /**
    * @param that polynomial to be added
    * @return new polynomial that is equal to this + that
    */
  def +(that: Polynomial): Polynomial = Polynomial(this.scalars
    .zipAll(that.scalars, BigDecimal(0), BigDecimal(0))
    .map({ case (x, y) => x + y }))

  def *(that: Polynomial): Polynomial = {
    if (this.isConst)
      that.scaledInternal(this.scalars.head)
    else if (that.isConst)
      this.scaledInternal(that.scalars.head)
    else {
      val productScalars = Array.fill[BigDecimal](this.level + that.level + 1)(0)
      for (i <- 0 to this.level)
        for (j <- 0 to that.level) {
          productScalars(i + j) += this.scalars(i) * that.scalars(j)
        }
      Polynomial(productScalars.toList)
    }
  }

  override def stringify(format: Format): String = {
    if (isConst)
      return format.num(const.get)
    val sb = StringBuilder.newBuilder
    for (i <- level to 0 by -1) {
      val scalar = scalars(i)
      if (scalar != 0) {
        if (scalar > 0)
          sb.append("+")
        if (i > 0)
          sb.append(format.scalar(scalar))
        else
          sb.append(format.num(scalar))
        if (i == 1)
          sb.append("x")
        else if (i > 1) sb.append(s"x^$i")
      }
    }
    val res = sb.toString()
    if (res.startsWith("+")) res.substring(1) else res
  }

  override def equals(that: Function): Boolean = that match {
    case other: Polynomial =>
      // todo correct
      this.level == other.level && this.scalars == other.scalars
    case _ => false
  }
}

object Polynomial {
  def apply() = new Polynomial(List(0))

  def apply(first: BigDecimal, many: BigDecimal*) = new Polynomial(first :: many.toList)

  def apply(scalars: List[BigDecimal]) = new Polynomial(scalars)
}
