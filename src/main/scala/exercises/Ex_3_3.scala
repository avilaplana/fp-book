package exercises

import scala.util.{Failure, Try}

//Using the same idea, implement the function setHead for replacing
// the first element of a List with a different value
object Ex_3_3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def setHead[A](h: A, l: List[A]) = Cons(h, l)
    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._
    assert(setHead(5, List(1, 2, 3)) == List(5, 1, 2, 3))
    assert(setHead(5, Nil) == List(5))
    }
}
