package exercises.chapter2

import scala.annotation.tailrec

object Ex_2_2 {

  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    as match {
      case Array(a) => true
      case Array(a,b,_*) if ordered(a, b) => isSorted(as.tail, ordered)
      case _ => false
    }

  def main(args: Array[String]): Unit = {

    assert(isSorted(Array(1), (a: Int,b: Int) => a < b) == true)

    assert(isSorted(Array(1, 2), (a: Int,b: Int) => a < b) == true)

    assert(isSorted(Array(1,2,3,4,5), (a: Int,b: Int) => a < b) == true)

    assert(isSorted(Array(1,2,7,4,5), (a: Int,b: Int) => a < b) == false)

  }

}
