package exercises.folder4

//Write a generic function map2 that combines two Option values using a binary function.
// If either Option value is None, then the return value is too.
object Ex_4_3 {

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
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(a1 => b.map(b1 => f(a1, b1)))
  }

  def main(args: Array[String]): Unit = {
    import Option._

    assert(map2(Some(1), Some(2))((x, y) => x + y) == Some(3))
    assert(map2(None, Some(2))((x: Int, y: Int) => x + y) == None)
    assert(map2(Some(1), None)((x, y) => x + y) == None)
  }
}
