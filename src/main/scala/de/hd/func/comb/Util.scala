package de.hd.func.comb

import de.hd.func.{Func2Pow, Function}

import scala.reflect.ClassTag

/**
  * Utility for function composition
  *
  * @author Henrik Drefs
  */
object Util {
  /**
    * @param that     a list of functions
    * @param matching condition for a function to be accepted in left list
    * @param evidence evidence for match-case on type T
    * @tparam T a function subclass that is basic condition for left partition
    * @return two lists; left -> all functions in 'that' list of type T that match the condition given by param function 'matching';
    *         right -> all remaining functions of 'that' list
    */
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


  def powPartition[T <: Function](list: List[Function], matching: T => Boolean)(implicit ev: ClassTag[T]): (List[Func2Pow[T]], List[Function]) = {
    val emptyLists: (List[Func2Pow[T]], List[Function]) = (Nil, Nil)
    list.foldRight(emptyLists) { case (f, (matched, rest)) =>
      f match {
        case f2p: Func2Pow[T] => f2p.inner match {
          case t: T =>
            if (matching(t)) (f2p :: matched, rest)
            else (matched, t :: rest)
          case other: Function => (matched, other :: rest)
        }
        case other: Function => (matched, other :: rest)
      }
    }
  }

}
