package exercises

import scala.util.{Failure, Try}

//Implement the function tail for removing the first element of a List. Note that the function takes constant time.
// What are different choices you could make in your implementation if the List is Nil?
object Ex_3_2 {

  sealed trait List[+A] {
    def tail:List[A]
  }

  case object Nil extends List[Nothing] {
    override def tail: List[Nothing] = throw new UnsupportedOperationException("tail of empty list")
  }
  case class Cons[+A](head: A, t: List[A]) extends List[A] {
    override def tail: List[A] = t
  }

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {

    assert(List(1,2,3,4,5,6).tail == List(2,3,4,5,6))

    Try(Nil.tail) match {
      case Failure(e) if e.isInstanceOf[UnsupportedOperationException] => println("right exception")
      case _ => println("bad result")
    }
  }

}
