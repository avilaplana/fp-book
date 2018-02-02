package exercises.chapter5

//Write the function takeWhile for returning all starting elements of a Stream
// that match the given predicate.
object Ex_5_3 {

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

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
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
    assert(Stream.apply(1, 2, 3, 4, 5).takeWhile(_ < 4).toList == Stream.apply(1, 2, 3).toList)
    assert(Stream.apply(1, 2, 3, 4, 5).takeWhile(_ > 10) == Empty)
  }

}
