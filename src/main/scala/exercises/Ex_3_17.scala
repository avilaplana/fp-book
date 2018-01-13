package exercises

//Write a function that turns each value in a List[Double] into a String.
object Ex_3_17 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def mapToString(source: List[Double]): List[String] = source match {
      case Nil => Nil
      case Cons(x, Nil) => Cons(x.toString, Nil)
      case Cons(x, xs) => Cons(x.toString, mapToString(xs))
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(mapToString(List(1, 2)) == List("1.0", "2.0"))
    assert(mapToString(List(1, 2, 1, 3)) == List("1.0", "2.0", "1.0", "3.0"))
    assert(mapToString(Nil) == Nil)
  }
}
