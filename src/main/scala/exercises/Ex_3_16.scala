package exercises

//Write a function that transforms a list of integers by adding 1 to each element.
// (Reminder: this should be a pure function that returns a new List!)
object Ex_3_16 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  //  {
  //    def a(c: A) = ???
  //  }

  object List {

    def plusOne(l: List[Int]): List[Int] = {
      def add(source: List[Int]): List[Int] = source match {
        case Nil => source
        case Cons(x, xs) => Cons(x +  1, add(xs))
      }
      add(l)
    }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(plusOne(List(1, 2)) == List(2, 3))
    assert(plusOne(List(1, 2, 1, 3)) == List(2, 3, 2, 4))
    assert(plusOne(Nil) == Nil)
  }
}
