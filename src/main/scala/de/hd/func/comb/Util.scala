package de.hd.func.comb

import de.hd.func.Function

import scala.reflect.ClassTag

/**
  * Created by henri on 12/13/2016.
  */
object Util {
  def genPartition[T <: Function](that: List[Function], matching: T => Boolean)
                                 (implicit evidence: ClassTag[T]): (List[T], List[Function]) = {
    val emptyLists: (List[T], List[Function]) = (Nil, Nil)
    that.foldRight(emptyLists) { case (f, (matched, rest)) =>
      f match {
        case t: T =>
          if (matching(t)) (t :: matched, rest)
          else (matched, t :: rest)
        case other: Function => (matched, other :: rest)
      }
    }
  }

}
