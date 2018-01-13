package exercises

import scala.annotation.tailrec


//Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
object Ex_3_13 {

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

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def foldLeftNoTailRec[A, B](as: List[A], z: B)(f: (B, A) => B): B = ??? //todo

    def foldRightTailRec[A, B](as: List[A], z: B)(f: (A, B) => B): B = ??? //todo



    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(foldLeftNoTailRec(List(1, 2, 3, 4), 1)(_ * _) == 24)

  }
}
