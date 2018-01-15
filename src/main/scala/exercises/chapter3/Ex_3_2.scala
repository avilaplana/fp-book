package exercises.chapter3

import scala.util.{Failure, Try}

//Implement the function tail for removing the first element of a List. Note that the function takes constant time.
// What are different choices you could make in your implementation if the List is Nil?
object Ex_3_2 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def tail[A](l: List[A]): List[A] = l match {
      case Nil => throw new UnsupportedOperationException("no tail in an empty list")
      case Cons(_, t) => t
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {

    import List._
    assert(tail(List(1,2,3,4,5,6)) == List(2,3,4,5,6))

    Try(tail(Nil)) match {
      case Failure(e) if e.isInstanceOf[UnsupportedOperationException] => println("right exception")
      case _ => println("bad result")
    }
  }
}
