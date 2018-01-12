package exercises

import exercises.Ex_3_2.List.tail
import exercises.Ex_3_2.{Cons, List, Nil}

import scala.annotation.tailrec
import scala.util.{Failure, Try}

//Generalize tail to the function drop, which removes the first n elements from a list.
//Note that this function takes time proportional only to the number of elements being dropped—we don’t
// need to make a copy of the entire List.
object Ex_3_4 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def tail[A](l: List[A]): List[A] = l match {
      case Nil => throw new UnsupportedOperationException("no tail in an empty list")
      case Cons(_, t) => t
    }

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil if n == 0 => Nil
      case Nil if n > 0 => throw new UnsupportedOperationException("List size is smaller than element to drop")
      case list if n == 0 => list
      case list => drop(tail(list), n - 1)
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._
    assert(drop(List(1, 2, 3), 2) == List(3))
    assert(drop(List(1, 2, 3), 3) == Nil)
    Try(drop(Nil, 2)) match {
      case Failure(e) if e.isInstanceOf[UnsupportedOperationException] => println("right exception")
      case _ => println("bad result")
    }
  }
}
