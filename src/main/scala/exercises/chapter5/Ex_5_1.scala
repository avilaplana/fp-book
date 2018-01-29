package exercises.chapter5


//Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in the REPL.
// You can convert to the regular List type in the standard library. You can place this and other functions that operate on a Stream
// inside the Stream trait.
object Ex_5_1 {

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = t1
      Cons(() => head, () => tail)
    }

    def empty[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  }

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3, 4, 5, 6).toList == List(1, 2, 3, 4, 5, 6))
    assert(Stream.empty.toList == Nil)
  }

}
