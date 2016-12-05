package func.trig

import func._
import func.FuncUtils._

/**
  * Created by Henrik on 6/20/2016.
  */
case class SineFunction private[func]() extends TrigonometricFunction {

  override val name: String = "sin"

  override def get(x: BigDecimal): BigDecimal = {
    scalar * Math.sin(x.toDouble)
  }

  override def derive(): Function = {
    Function.cos(scalar)
  }

  override def antiderive(c: BigDecimal): Function = {
    Function.cos(-scalar) + c
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case sine: SineFunction =>
      // a*sin(x)=b*sin(x), if a=b
      this.scalar == sine.scalar
    case concat: ConcatFunction =>
      concat.outer match {
        case sine: SineFunction =>
          if (!concat.inner.isInstanceOf[Polynomial]) {
            return false
          }
          val inner = concat.inner.asInstanceOf[Polynomial]
          // a*sin(x) = a*sin(x + k*2pi), k is int
          (scalar == sine.scalar && inner.isLinear
            && inner.scalars(0) % (Math.PI * 2) == 0 && inner.scalars(1) == 1) ||
            // -a*sin(x) = a*sin(-x)
            (-scalar == sine.scalar && inner.isLinear
              && inner.scalars(0) == 0 && inner.scalars(1) == -1)
        case cosine: CosineFunction =>
          if (!concat.inner.isInstanceOf[Polynomial]) {
            return false
          }
          val inner = concat.inner.asInstanceOf[Polynomial]
          inner.isLinear &&
            // a*sin(x)=a*cos(pi/2-x)
            ((scalar == cosine.scalar && inner.scalars(0) == Math.PI / 2 && inner.scalars(1) == -1) ||
              // a*sin(x)= -a*cos(x+pi/2)
              (scalar == -cosine.scalar && inner.scalars(0) == Math.PI / 2 && inner.scalars(1) == 1) ||
              // a*sin(x) = a*cos(x-pi/2)
              (scalar == cosine.scalar && inner.scalars(0) == -Math.PI / 2) && inner.scalars(1) == 1)
        case _ =>
          false
      }
    case comb: CombinedFunction =>
      comb.operator == Operator.TIMES && (comb.f1 match {
        case tan: TangentFunction =>
          // a*sin(x)=b*tan(x)*c*cos(x), if b*c=a
          comb.f2.isInstanceOf[CosineFunction] &&
            comb.f2.asInstanceOf[CosineFunction].scalar * tan.scalar == scalar
        case cos: CosineFunction =>
          // a*sin(x)=b*cos(x)*c*tan(x), if b*c=a
          comb.f2.isInstanceOf[TangentFunction] &&
            comb.f2.asInstanceOf[TangentFunction].scalar * cos.scalar == scalar
        case _ => false
      })
    case _ =>
      false
  }
}
