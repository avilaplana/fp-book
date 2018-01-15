package exercises

import scala.util.{Failure, Try}

//Generalize the function you just wrote so that
// it’s not specific to integers or addition. Name your generalized function zipWith.
object Ex_3_23 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Nil, _) => throw new UnsupportedOperationException("list with differnt size")
      case (_, Nil) => throw new UnsupportedOperationException("list with differnt size")
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

        assert(zipWith(List(1, 3, 5), List(2, 2, 6))((a,b) => a + b) == List(3, 5, 11))
        assert(zipWith(List("alvaro", "mercedes", "london"), List(2, 2, 6))((a,b) => a.length + b) == List(8, 10, 12))
        assert(zipWith[Int,Int, Int](Nil, Nil)((a,b) => a + b) == Nil)

        Try(zipWith(List(1, 3, 5, 6), List(2, 2, 6))((a,b) => a + b)) match {
          case Failure(e) if e.isInstanceOf[UnsupportedOperationException] => println("right exception")
          case _ => println("bad result")
        }

        Try(zipWith(List(1, 3, 5), List(2, 2, 6, 7))((a,b) => a + b)) match {
          case Failure(e) if e.isInstanceOf[UnsupportedOperationException] => println("right exception")
          case _ => println("bad result")
        }
  }
}
