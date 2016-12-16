package de.hd.func.impl.comb

import de.hd.func
import de.hd.func.impl.log.LogBaseFunction
import de.hd.func.impl.{Func2Pow, Polynomial, RootFunction}
import de.hd.func.impl.trig.TrigonometricFunction
import de.hd.func.{Format, Function, GenFunction, ScalarFunction}

import scala.reflect.ClassTag

/**
  * A sum of functions
  */
case class FunctionSum(private val addends: List[Function]) extends Function {

  override def +(that: Function): Function = that match {
    case sum: FunctionSum =>
      this ++ sum.addends
    case exp: de.hd.func.impl.exp.ExponentialFunction =>
      scaleOrAdd[de.hd.func.impl.exp.ExponentialFunction](exp, _.base, _.inner)
    case log: LogBaseFunction =>
      scaleOrAdd[LogBaseFunction](log, _.base)
    case trig: TrigonometricFunction =>
      mergeOrAdd[TrigonometricFunction](trig, _.getClass)((that, f) =>
        that.scaledInternal((that.scalar + f.scalar) / that.scalar).asInstanceOf[TrigonometricFunction])
    case f2p: Func2Pow[Function] =>
      scaleOrAdd[Func2Pow[Function]](f2p, _.inner, _.n)
    case poly: Polynomial =>
      mergeOrAdd[Polynomial](poly)((that, f) => that + f)
    case root: RootFunction =>
      scaleOrAdd[RootFunction](root, _.n)
    case any: Function =>
      FunctionSum(any :: addends)
  }

  def ++(those: List[Function]): FunctionSum = {
    var sum: Function = this
    for (addend <- those) sum += addend
    sum.asInstanceOf[FunctionSum]
  }

  private def mergeOrAdd[T <: Function](that: T, values: (T => Any)*)
                                       (merge: (T, T) => T)
                                       (implicit evidence: ClassTag[T]): FunctionSum = {
    //    val (matched, rest) = addends.partition(f => f.isInstanceOf[T] && (values.isEmpty || values.forall(v => v(f) == v(that))))
    val (matched, rest) = Util.genPartition[T](addends, f => values.isEmpty || values.forall(get => get(f) == get(that)))
    if (matched.size == 1)
      FunctionSum(merge(matched.head, that) :: rest)
    else FunctionSum(that :: rest)
  }

  /**
    * merges through adding both scalars
    *
    * @param that     function to merge into
    * @param values   values to be checked for equality
    * @param evidence class tag for generic type
    * @tparam T a scalable function type
    */
  private def scaleOrAdd[T <: ScalarFunction](that: T, values: (T => Any)*)
                                             (implicit evidence: ClassTag[T], gf: GenFunction[T] = that): FunctionSum =
    mergeOrAdd[T](that, values: _*)((that, f) => gf.scaledInternal(that, (that.scalar + f.scalar) / that.scalar))

  //    mergeOrAdd[T](that, values: _*)((that, f) => that.scaled((that.scalar + f.scalar) / that.scalar).asInstanceOf[T])

  override def get(x: BigDecimal): BigDecimal = addends.map(_.get(x)).sum

  override protected def derive(): Function = FunctionSum(addends.map(_.derivative))

  override def antiderive(c: BigDecimal): Function = FunctionSum(addends.map(_.antiderive(c)))

  override def getConst: Option[BigDecimal] = {
    var totalConst: BigDecimal = 0
    for (const <- addends.map(_.const)) {
      if (const.isDefined)
        totalConst += const.get
      else
        return None
    }
    Some(totalConst)
  }

  override protected def simplify: Function = {
    // todo merge all const members
    val filtered = addends.filter(!_.const.contains(0))
    if (filtered.size == 1)
      return filtered.head
    val (constants, rest) = Util.genPartition[Function](filtered, _.isConst)
    val const = constants.map(_.const.get).sum
    if (const != 0)
      FunctionSum(Function.const(const) :: rest)
    else FunctionSum(rest)
  }

  override def stringify(format: Format): String = addends.filter(!_.const.contains(0)).map(_.stringify(format)).mkString("+")

  override def equals(that: Function): Boolean = simplify match {
    case sum: FunctionSum => that match {
      case thatSum: FunctionSum =>
        sum.addends.toSet == thatSum.addends.toSet
      case _ => false
    }
    case _ => simplify == that
  }

  override def scaledInternal(scalar: BigDecimal): FunctionSum = FunctionSum(addends.map(_.scaledInternal(scalar)))
}

object FunctionSum {

  def apply(): FunctionSum = new FunctionSum(List(Function.const(0)))

  def apply(first: Function): FunctionSum = first match {
    case sum: FunctionSum => new FunctionSum(sum.addends)
    case _ => new FunctionSum(List(first))
  }

}
