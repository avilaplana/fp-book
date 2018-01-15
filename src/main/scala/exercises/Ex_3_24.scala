package exercises

import scala.util.{Failure, Try}

//Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence.
// For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
object Ex_3_24 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ??? // todo

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._


  }
}
