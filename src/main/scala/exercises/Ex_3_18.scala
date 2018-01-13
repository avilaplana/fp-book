package exercises

//Write a function map that generalizes modifying each element
// in a list while maintaining the structure of the list.
object Ex_3_18 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def map[A, B](source: List[A])(f: A => B): List[B] = source match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(map(List(1, 2))(x => x.toString) == List("1", "2"))
    assert(map(List(1, 2, 1, 3))(x => x + 1) == List(2, 3, 2, 4))
    assert(map[Int, Int](Nil)(x => x + 1) == Nil)
    // todo type inference for higher-order functions in this version of scala?
  }
}
