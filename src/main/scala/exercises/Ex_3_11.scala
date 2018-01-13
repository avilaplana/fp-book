package exercises

import scala.annotation.tailrec

//Write sum, product, and a function to compute the length of a list using foldLeft.
object Ex_3_11 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(foldLeft(List(1, 2, 3, 4), 1)(_ * _) == 24)
    assert(foldLeft(Nil: List[Int], 1)(_ + _) == 1)
  }
}
