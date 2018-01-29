package exercises.chapter5


//Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first n elements of a Stream.
object Ex_5_2 {

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case Cons(h, t) => this
      case _ => Empty
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
    assert(Stream.apply(1, 2, 3, 4, 5).take(2).toList == Stream.apply(1, 2).toList)
    assert(Stream.empty.take(6) == Stream.empty)
    assert(Stream.apply(1, 2, 3, 4, 5).take(6).toList == Stream.apply(1, 2, 3, 4, 5).toList)

    assert(Stream.apply(1, 2, 3, 4, 5).drop(2).toList == Stream.apply(3, 4, 5).toList)
    assert(Stream.empty.drop(2) == Stream.empty)
    assert(Stream.apply(1, 2, 3, 4, 5).drop(10) == Stream.empty)

  }

}
