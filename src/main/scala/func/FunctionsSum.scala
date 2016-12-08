package func

import func.exp.ExponentialFunction
import func.log.LogBaseFunction
import func.trig.TrigonometricFunction

import scala.reflect.ClassTag

/**
  * Created by henri on 12/5/2016.
  */
case class FunctionsSum private[func](private var addends: List[Function]) extends Function {

  override def +(that: Function): Function = this.clone().asInstanceOf[FunctionsSum].add(that)

  def add(that: Function): FunctionsSum = {
    that match {
      case sum: FunctionsSum =>
        sum.addends.foreach(add)
      case exp: ExponentialFunction =>
        // merge if base & inner are equal, else just add to list
        scaleOrAdd[ExponentialFunction](exp, _.base, _.inner)
      case log: LogBaseFunction =>
        scaleOrAdd[LogBaseFunction](log, _.base)
      case trig: TrigonometricFunction =>
        scaleOrAdd[TrigonometricFunction](trig, _.getClass)
      case f2p: Func2Pow =>
        scaleOrAdd[Func2Pow](f2p, _.inner, _.n)
      case poly: Polynomial =>
        mergeOrAdd[Polynomial](poly)((that, f) => that.add(f))
      case root: RootFunction =>
        scaleOrAdd[RootFunction](root, _.n)
      case any: Function =>
        addends = any :: addends
    }
    this
  }

  private def mergeOrAdd[T <: Function](that: T, values: (T => Any)*)(merge: (T, T) => Unit)(implicit ev: ClassTag[T]): Unit = {
    var f = addends.collect { case t: T => t }
    f = f.filter(t => values.isEmpty || values.forall(value => value(t) == value(that)))
    if (f.size == 1)
      merge(f.head, that)
    else addends = that :: addends
  }

  /**
    * merges through adding both scalars
    *
    * @param that   function to merge into
    * @param values values to be checked for equality
    * @param ev     class tag for generic type
    * @tparam T a scalable function type
    */
  private def scaleOrAdd[T <: ScalableFunction](that: T, values: (T => Any)*)(implicit ev: ClassTag[T]): Unit = {
    mergeOrAdd[T](that, values: _*)((that, f) => that.setScalar(that.scalar + f.scalar))
  }

  override def get(x: BigDecimal): BigDecimal = addends.map(_.get(x)).sum

  override def scale(factor: BigDecimal): Unit = addends.foreach(_.scale(factor))

  override def scaled(factor: BigDecimal): Function = FunctionsSum(addends.map(_ * factor))

  override def derive(): Function = FunctionsSum(addends.map(_.derive()))

  override def antiderive(c: BigDecimal): Function = FunctionsSum(addends.map(_.antiderive(c)))

  override def constValue: Option[BigDecimal] = {
    var totalConst: BigDecimal = 0
    for (const <- addends.map(_.constValue)) {
      if (const.isDefined)
        totalConst += const.get
      else
        return None
    }
    Some(totalConst)
  }

  override def simplified: Function = {
    val filtered = addends.filter(!_.constValue.contains(0))
    if (filtered.size == 1)
      filtered.head
    else FunctionsSum(filtered)
  }

  override def stringify(format: Format): String = addends.filter(!_.constValue.contains(0)).map(_.stringify(format)).mkString("+")

  override def equals(that: Function): Boolean = simplified match {
    case sum: FunctionsSum => that match {
      case thatSum: FunctionsSum =>
        sum.addends.toSet == thatSum.addends.toSet
      case _ => false
    }
    case _ => simplified.equals(that)
  }
}

object FunctionsSum {
  def apply(): FunctionsSum = new FunctionsSum(List(Function.const(0)))

  def apply(first: Function): FunctionsSum = first match {
    case sum: FunctionsSum => new FunctionsSum(sum.addends)
    case _ => new FunctionsSum(List(first))
  }
}
