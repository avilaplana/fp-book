package exercises

import exercises.Ex_3_9.{Cons, List, Nil}

import scala.annotation.tailrec

//write another general list-recursion function, foldLeft, that is tail-recursive
object Ex_3_10 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

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

    //    val list: List[Int] = List(Range(1, Int.MaxValue):_*)
    //    foldRight(list, 0)(_ + _) => java.lang.StackOverflowError

    assert(foldLeft(List(1, 2, 3), 0)(_ + _) == 6)
    assert(foldLeft(Nil: List[Int], 0)(_ + _) == 0)
  }
}
