package exercises

//Hard: Write a function that concatenates a list of lists into a single list.
// Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.

object Ex_3_15 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def concat[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_,_))

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    println(concat(List(1, 2), List(3, 4)))
    assert(concat(List(1, 2), List(3, 4)) == List(1, 2, 3, 4))
    assert(concat(List(1, 2), Nil) == List(1, 2))
    assert(concat(Nil, List(1, 2)) == List(1, 2))
    assert(concat(Nil, Nil) == Nil)
  }
}
