package exercises

//Write a function filter that removes elements from a list unless they satisfy a given predicate.
//Use it to remove all odd numbers from a List[Int].
object Ex_3_19 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def filter[A](source: List[A])(f: A => Boolean): List[A] = source match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      case Cons(x, xs) => filter(xs)(f)
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(filter(List("al", "alvaro", "putney", "london"))(x => x.startsWith("al")) == List("al", "alvaro"))
    assert(filter(List(1, 3, 5, 6,8, 10))(x => x % 2 == 0 ) == List(6, 8, 10))
  }
}
