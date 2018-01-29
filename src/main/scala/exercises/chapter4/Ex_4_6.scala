package exercises.chapter4

//Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
object Ex_4_6 {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => Right(a)
      case Left(e) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]


  def main(args: Array[String]): Unit = {

    assert(Right(1).map(_ + 1) == Right(2))
    val e1: Either[String, Int] = Left("London")
    assert(e1.map(_ + 1) == Left("London"))

    assert(Right(1).flatMap(r => Right(r + 1)) == Right(2))
    val e2: Either[String, Int] = Left("London")
    assert(e2.flatMap(r => Right(r + 1)) == Left("London"))

    assert(Right(1).orElse(Right("london")) == Right(1))
    val e3: Either[String, Int] = Left("London")
    assert(e3.orElse(Right(1)) == Right(1))
    assert(e3.orElse(Left(2)) == Left(2))

    assert(Right(1).map2(Right(2))(_ + _) == Right(3))
    val e4: Either[String, Int] = Left("London")
    assert(e4.map2(Right(2))(_ + _) == Left("London"))
    assert(Right(1).map2(Left(2))(_ + _) == Left(2))
  }
}
