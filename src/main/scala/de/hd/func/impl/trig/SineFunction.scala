package de.hd.func.impl.trig

import de.hd.func._
import de.hd.func.impl.{CompositeFunction, Polynomial}

/**
  * the sine function
  */
case class SineFunction private[func](override val scalar: BigDecimal = 1) extends GenTrigonometricFunction[SineFunction](scalar) {

  override val name: String = "sin"

  override def get(x: BigDecimal): BigDecimal = scalar * Math.sin(x.toDouble)

  override def derive(): Function = Function.cos(scalar)

  override def antiderive(c: BigDecimal): Function = Function.cos(-scalar) + Function.const(c)

  override def withScalar(newScalar: BigDecimal): SineFunction = copy(scalar = newScalar)

  override def equals(that: Function): Boolean = that match {
    case sine: SineFunction =>
      // a*sin(x)=b*sin(x), if a=b
      this.scalar == sine.scalar
    case concat: CompositeFunction =>
      concat.outer match {
        case sine: SineFunction =>
          if (!concat.inner.isInstanceOf[Polynomial]) {
            return false
          }
          val inner = concat.inner.asInstanceOf[Polynomial]
          // a*sin(x) = a*sin(x + k*2pi), k is int
          (scalar == sine.scalar && inner.isLinear
            && inner.scalars.head % (Math.PI * 2) == 0 && inner.scalars(1) == 1) ||
            // -a*sin(x) = a*sin(-x)
            (-scalar == sine.scalar && inner.isLinear
              && inner.scalars.head == 0 && inner.scalars(1) == -1)
        case cosine: CosineFunction =>
          if (!concat.inner.isInstanceOf[Polynomial]) {
            return false
          }
          val inner = concat.inner.asInstanceOf[Polynomial]
          inner.isLinear &&
            // a*sin(x)=a*cos(pi/2-x)
            ((scalar == cosine.scalar && inner.scalars.head == Math.PI / 2 && inner.scalars(1) == -1) ||
              // a*sin(x)= -a*cos(x+pi/2)
              (scalar == -cosine.scalar && inner.scalars.head == Math.PI / 2 && inner.scalars(1) == 1) ||
              // a*sin(x) = a*cos(x-pi/2)
              (scalar == cosine.scalar && inner.scalars.head == -Math.PI / 2) && inner.scalars(1) == 1)
        case _ =>
          false
      }
    case _ =>
      false
  }
}
