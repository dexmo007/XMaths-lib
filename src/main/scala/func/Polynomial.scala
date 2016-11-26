package func

import func.FuncUtils.MathString

/**
  * Created by Henrik on 6/20/2016.
  */
case class Polynomial(scls: BigDecimal*) extends Function {

  var scales: Array[BigDecimal] = {
    if (scls.isEmpty) {
      Array(0)
    } else {
      scls.toArray
    }
  }

  var level: Int = scales.length - 1

  def get(x: BigDecimal): BigDecimal = {
    var res: BigDecimal = 0
    for (k <- 0 to level) {
      res += scales(k) * x.pow(k)
    }
    res
  }

  override def scale(factor: BigDecimal) {
    scales = scales.map(x => factor * x)
  }

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

  def isLinear: Boolean = {
    if (level <= 1) return true
    for (i <- 2 to level) {
      if (scales(i) != 0) {
        return false
      }
    }
    true
  }

  def getFirstEffectiveScale: BigDecimal = {
    for (scale <- scales) {
      if (scale != 0) {
        return scale
      }
    }
    0
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
