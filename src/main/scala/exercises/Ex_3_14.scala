package exercises

import scala.annotation.tailrec

//Implement append in terms of either foldLeft or foldRight. // todo review this

object Ex_3_14 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def append1[A](as: List[A], z: A): List[A] = as match {
      case Nil => Cons(z, Nil)
      case Cons(x, Nil) => Cons(x, Cons(z, Nil))
      case Cons(x, xs) => Cons(x, append1(xs, z))
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(append1(List(1, 2, 3, 4), 5) == List(1, 2, 3, 4, 5))
    assert(append1(Nil, 5) == List(5))

  }
}
