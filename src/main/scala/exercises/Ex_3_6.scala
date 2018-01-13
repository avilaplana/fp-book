package exercises

import scala.util.{Failure, Try}

//Implement a function, init, that returns a List
// consisting of all but the last element of a List


object Ex_3_6 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def init[A](l: List[A]): List[A]= l match {
      case Nil => throw new UnsupportedOperationException("no init in an empty list")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List.init
    assert(init(List(1,2,3,4,5)) == List(1,2,3,4))
    assert(init(List(1)) == Nil)
    Try(init(Nil)) match {
      case Failure(e) if e.isInstanceOf[UnsupportedOperationException] => println("right exception")
      case _ => println("bad result")
    }
  }


}
