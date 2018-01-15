package exercises

import exercises.Ex_3_6.List.init
import exercises.Ex_3_6.Nil

import scala.util.{Failure, Try}

//Write a function that accepts two lists and constructs a new list by adding corresponding elements.
// For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
object Ex_3_22 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def add(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Nil, _) =>  throw new UnsupportedOperationException("list with differnt size")
      case (_, Nil) =>  throw new UnsupportedOperationException("list with differnt size")
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, add(xs1, xs2))
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(add(List(1, 3, 5), List(2, 2, 6)) == List(3, 5, 11))
    assert(add(Nil, Nil) == Nil)

    Try(add(List(1, 3, 5, 6), List(2, 2, 6))) match {
      case Failure(e) if e.isInstanceOf[UnsupportedOperationException] => println("right exception")
      case _ => println("bad result")
    }

    Try(add(List(1, 3, 5), List(2, 2, 6, 7))) match {
      case Failure(e) if e.isInstanceOf[UnsupportedOperationException] => println("right exception")
      case _ => println("bad result")
    }
  }
}
