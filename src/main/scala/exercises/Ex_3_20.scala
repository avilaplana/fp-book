package exercises

//Write a function flatMap that works like map except that the function given will return a list
// instead of a single result, and that list should be inserted into the final resulting list
object Ex_3_20 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = ??? // todo


    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(flatMap(List(1, 3, 5, 6, 8, 10))(x => List(x, x + 1)) == List(1, 2, 3, 4, 5, 6, 6, 7, 8, 9, 10, 11))
    assert(flatMap[Int, Int](Nil)(x => List(x, x + 1)) == Nil)
  }
}
