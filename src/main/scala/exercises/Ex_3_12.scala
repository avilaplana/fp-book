package exercises

import exercises.Ex_3_10.{Cons, List, Nil}

import scala.annotation.tailrec

//Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)). See if you can write it using a fold.
object Ex_3_12 {

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

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(foldLeft(List(1, 2, 3, 4), Nil:List[Int])((a,b) => Cons(b,a)) == List(4, 3, 2, 1))
    assert(foldLeft(Nil, Nil:List[Int])((a,b) => Cons(b,a)) == Nil)
  }
}
