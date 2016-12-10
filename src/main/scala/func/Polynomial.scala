package func

import func.FuncUtils._

/**
  * Created by Henrik on 6/20/2016.
  */
case class Polynomial(private val _scalars: BigDecimal*) extends Function with GenCloneable[Polynomial] {

  var scalars: Array[BigDecimal] = {
    if (_scalars.isEmpty) {
      Array(0)
    } else {
      _scalars.toArray
    }
  }

  def level: Int = {
    for (i <- scalars.indices.reverse)
      if (scalars(i) != 0)
        return i
    0
  }

  def get(x: BigDecimal): BigDecimal = {
    var res: BigDecimal = 0
    for (k <- 0 to level) {
      res += scalars(k) * x.pow(k)
    }
    res
  }

  override def scaleInternal(factor: BigDecimal) {
    scalars = scalars.map(_ * factor)
  }

  override def derive(): Function = {
    val buf = scalars.toBuffer
    for (i <- 2 to level) {
      buf(i) *= i
    }
    buf.remove(0)
    Polynomial(buf: _*)
  }

  override def antiderive(c: BigDecimal): Function = {
    val buf = scalars.toBuffer
    for (i <- 0 to level) {
      buf(i) /= (i + 1)
    }
    buf.insert(0, c)
    Polynomial(buf: _*)
  }

  override def constValue: Option[BigDecimal] = {
    if (level == 0 || scalars.drop(1).forall(n => n == 0))
      Some(scalars(0))
    else None
  }

  override def isLinear: Boolean = isConst || level < 2 || scalars.drop(2).forall(n => n == 0)

  def getFirstEffectiveScale: BigDecimal = {
    for (scale <- scalars) {
      if (scale != 0) {
        return scale
      }
    }
    0
  }

  override def +(that: Function): Function = that match {
    case p: Polynomial => this + p
    case _ => super.+(that)
  }

  /**
    * @param that polynomial to be added
    * @return new polynomial that is equal to this + that
    */
  def +(that: Polynomial): Polynomial = Polynomial(scalars: _*).add(that)

  /**
    * mutates this instance through adding the other polynomial to this
    *
    * @param that polynomial to add
    * @return this instance
    */
  def add(that: Polynomial): Polynomial = {
    this.scalars = this.scalars.zipAll(that.scalars, BigDecimal(0), BigDecimal(0)).map({ case (x, y) => x + y })
    this
  }

  def *(that: Polynomial): Polynomial = {
    if (this.isConst)
      that.scaled(this.scalars(0)).asInstanceOf[Polynomial]
    else if (that.isConst)
      this.scaled(that.scalars(0)).asInstanceOf[Polynomial]
    else {
      val productScalars = Array.fill[BigDecimal](this.level + that.level + 1)(0)
      for (i <- 0 to this.level)
        for (j <- 0 to that.level) {
          productScalars(i + j) += this.scalars(i) * that.scalars(j)
        }
      Polynomial(productScalars: _*)
    }
  }

  override def *(that: Function): Function = {
    if (this.isConst)
      that * this.scalars(0)
    else if (that.isConst)
      this * that.constValue.get
    else
      that match {
        case p: Polynomial => this * p
        case _ => super.*(that)
      }
  }

  override def stringify(format: Format): String = {
    if (isConst)
      return format.num(constValue.get)
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
      this.level == other.level && this.scalars.deep == other.scalars.deep
    case _ => false
  }
}
