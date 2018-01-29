package exercises.chapter4

//Implement this function. It’s straightforward to do using map and sequence, but try for a more efficient
// implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
  object Ex_4_5 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def orElse[B >: A](default: => Option[B]): Option[B] = this match {
      case None => default
      case Some(v) => Some(v)
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case None => None
      case Some(v) if f(v) => Some(v)
      case _ => None
    }

    def get: A = this match {
      case None => throw new RuntimeException("there is no value")
      case Some(v) => v
    }

  }

  case object None extends Option[Nothing]

  case class Some[A](value: A) extends Option[A]

  object Option {
//    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(a1 => b.map(b1 => f(a1, b1)))

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ??? //todo

  }

  def main(args: Array[String]): Unit = {
    import Option._

//    assert(sequence(List(Some(1), Some(2), Some(3), Some(4))) == Some(Seq(1, 2, 3, 4)))
//    assert(sequence(List(Some(1), None, Some(3), Some(4))) == None)
  }
}
