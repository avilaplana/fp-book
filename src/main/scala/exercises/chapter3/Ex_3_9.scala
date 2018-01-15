package exercises.chapter3

//Compute the length of a list using foldRight.

object Ex_3_9 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(foldRight(List(1, 2, 3), 0)((a,b) => 1 + b) == 3)
    assert(foldRight(Nil, 0)((a,b) => 1 + b) == 0)
  }
}
