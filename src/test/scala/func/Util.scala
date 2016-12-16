package func

import scala.reflect.ClassTag

/**
  * Created by henri on 12/16/2016.
  */
object Util {
  def doubleAndPartition[T <: GenFunction[T] : ClassTag](list: List[MathFunction]): (List[T], List[MathFunction]) = {
    val emptyLists: (List[T], List[MathFunction]) = (Nil, Nil)
    list.foldRight(emptyLists) { case (f, (matching, rest)) =>
      f match {
        case t: T => ((t * 2) :: matching, rest)
        case _ => (matching, (f * 2) :: rest)
      }
    }
  }

  def powPartition[T <: GenFunction[T] : ClassTag](list: List[MathFunction]): (List[Func2Pow], List[MathFunction]) = {
    val emptyLists: (List[Func2Pow], List[MathFunction]) = (Nil, Nil)
    list.foldRight(emptyLists) { case (f, (matched, rest)) =>
      f match {
        case f2p: Func2Pow => f2p.inner match {
          case t: T => (f2p :: matched, rest)
          case _ => (matched, f :: rest)
        }
        case _ => (matched, f :: rest)
      }
    }
  }
}
