package func

import func.FuncUtils.MathString

/**
  * Created by Henrik on 6/20/2016.
  */
case class Polynomial(private val _scales: BigDecimal*) extends Function {

  var scalars: Array[BigDecimal] = {
    if (_scales.isEmpty) {
      Array(0)
    } else {
      _scales.toArray
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

  override def scale(factor: BigDecimal) {
    scalars = scalars.map(_ * factor)
  }

  override def scaled(factor: BigDecimal) = Polynomial(scalars.map(_ * factor): _*)

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

  def +(that: Polynomial): Polynomial = {
    val merged = this.scalars.zipAll(that.scalars, BigDecimal(0), BigDecimal(0)).map({ case (x, y) => x + y })
    Polynomial(merged: _*)
  }

  def -(that: Polynomial): Polynomial = {
    val merged = this.scalars.zipAll(that.scalars, BigDecimal(0), BigDecimal(0)).map({ case (x, y) => x - y })
    Polynomial(merged: _*)
  }

  override def -(that: Function): Function = that match {
    case p: Polynomial => this - p
    case _ => super.+(that)
  }

  override def *(that: Function): Function = {
    if (this.isConst) {
      that.scaled(this.scalars(0))
    } else {
      that match {
        case p: Polynomial =>
          if (p.isConst)
            this.scaled(p.scalars(0))
          else
            super.*(that)
        case _ => super.*(that)
      }
    }
  }

  override def toString: String = {
    val sb = StringBuilder.newBuilder
    if (scalars(0) != 0) {
      sb.append(scalars(0))
    }
    for (i <- 1 to level) {
      val scale = scalars(i)
      if (scale != 0) {
        if (scale == -1) {
          sb.append("-x")
        } else if (scale < 0) {
          sb.append("-" + scale + "*x")
        } else if (sb.nonEmpty) {
          sb.append("+" + scale.toScalarString + "x")
        } else {
          sb.append(scale.toScalarString + "x")
        }
        if (i != 1) {
          sb.append("^" + i)
        }
      }
    }
    sb.toString
  }

  override def toTexString: String = {
    val sb = StringBuilder.newBuilder
    if (scalars(level) < 0) {
      sb.append("-")
    }
    for (i <- level to 0 by -1) {
      val scale = scalars(i)
      if (scale != 0) {
        if (scale != 1 && scale != -1) {
          sb.append(scale.abs.toTexString)
        }
        // append exponent
        if (i == 1) {
          sb.append("x")
        } else if (i > 1) {
          sb.append("x^" + i)
        }
        // todo check what happens if next scale is 0
        // append the sign for next scale
        if (i > 0) {
          val nextScale = scalars(i - 1)
          if (nextScale < 0) {
            sb.append("-")
          } else if (nextScale > 0) {
            sb.append("+")
          }
        }
      }
    }
    sb.toString
  }

  override def equals(obj: scala.Any): Boolean = {
    if (!obj.isInstanceOf[Polynomial])
      return false
    val other = obj.asInstanceOf[Polynomial]
    this.level == other.level && this.scalars.deep == other.scalars.deep
  }
}
