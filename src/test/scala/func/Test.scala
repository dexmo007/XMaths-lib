package func

import scala.collection.mutable.ListBuffer

/**
  * Created by henri on 12/17/2016.
  */
object Test {

  trait MathSeq extends (Int => BigInt) {
    def apply(i: Int): BigInt
  }

  abstract class TailRecAdd2PrevSeq(val first: BigInt, val second: BigInt) extends MathSeq {
    override def apply(i: Int): BigInt = {

      def tailRec(n: Int, a: BigInt, b: BigInt): BigInt = n match {
        case 0 => a
        case _ => tailRec(n - 1, b, a + b)
      }

      tailRec(i, first, second)
    }
  }

  abstract class StoredAdd2PrevSeq(val first: BigInt, val second: BigInt) extends MathSeq {

    private var seq: List[BigInt] = List(second, first)

    override def apply(i: Int): BigInt =
      if (seq.size > i) seq(seq.size - i - 1)
      else {
        val lb = new ListBuffer[BigInt]
        val firstNew = seq.head + seq.tail.head
        lb += firstNew
        lb.prepend(firstNew + seq.head)
        for (_ <- 0 to (i - seq.size))
          lb.prepend(lb.head + lb.tail.head)
        seq = lb.toList ++ seq
        seq(seq.size - i - 1)
      }

  }

  case class StoredFibonacci() extends StoredAdd2PrevSeq(0, 1)

  case class TailRecFibonacci() extends TailRecAdd2PrevSeq(0, 1)

  def main(args: Array[String]): Unit = {
    val fib = TailRecFibonacci()
    for (i <- 0 to 10)
      println(fib(i))
  }
}

