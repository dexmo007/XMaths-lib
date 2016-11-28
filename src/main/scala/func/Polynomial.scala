package func

import func.FuncUtils.MathString

/**
  * Created by Henrik on 6/20/2016.
  */
case class Polynomial(private val _scales: BigDecimal*) extends Function {

  var scales: Array[BigDecimal] = {
    if (_scales.isEmpty) {
      Array(0)
    } else {
      _scales.toArray
    }
  }

  // todo calculate dynamically - reverse iterate scalars until the scalar is not zero
  var level: Int = scales.length - 1

  def get(x: BigDecimal): BigDecimal = {
    var res: BigDecimal = 0
    for (k <- 0 to level) {
      res += scales(k) * x.pow(k)
    }
    res
  }

  override def scale(factor: BigDecimal) {
    scales = scales.map(_ * factor)
  }

  override def scaled(factor: BigDecimal) = Polynomial(scales.map(_ * factor): _*)

  override def derive(): Function = {
    val buf = scales.toBuffer
    for (i <- 2 to level) {
      buf(i) *= i
    }
    buf.remove(0)
    Polynomial(buf: _*)
  }

  override def antiderive(c: BigDecimal): Function = {
    val buf = scales.toBuffer
    for (i <- 0 to level) {
      buf(i) /= (i + 1)
    }
    buf.insert(0, c)
    Polynomial(buf: _*)
  }

  def isConst: Boolean = level == 0 || scales.drop(1).forall(n => n == 0)

  // todo move to function trait and change adequately
  def isLinear: Boolean = level < 2 || scales.drop(2).forall(n => n == 0)

  def getFirstEffectiveScale: BigDecimal = {
    for (scale <- scales) {
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
    val merged = this.scales.zipAll(that.scales, BigDecimal(0), BigDecimal(0)).map({ case (x, y) => x + y })
    Polynomial(merged: _*)
  }

  def -(that: Polynomial): Polynomial = {
    val merged = this.scales.zipAll(that.scales, BigDecimal(0), BigDecimal(0)).map({ case (x, y) => x - y })
    Polynomial(merged: _*)
  }

  override def -(that: Function): Function = that match {
    case p: Polynomial => this - p
    case _ => super.+(that)
  }

  override def *(that: Function): Function = {
    if (this.isConst) {
      that.scaled(this.scales(0))
    } else {
      that match {
        case p: Polynomial =>
          if (p.isConst)
            this.scaled(p.scales(0))
          else
            super.*(that)
        case _ => super.*(that)
      }
    }
  }

  override def toString: String = {
    val sb = StringBuilder.newBuilder
    if (scales(0) != 0) {
      sb.append(scales(0))
    }
    for (i <- 1 to level) {
      val scale = scales(i)
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
    if (scales(level) < 0) {
      sb.append("-")
    }
    for (i <- level to 0 by -1) {
      val scale = scales(i)
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
          val nextScale = scales(i - 1)
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
    this.level == other.level && this.scales.deep == other.scales.deep
  }
}
