package exercises.chapter4

//Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
object Ex_4_7 {

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

  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es.foldRight[Either[E, List[A]]](Right(List.empty[A])) {
      (a, b) => a.flatMap(aa => b.map(bb => aa :: bb))
    }

    def sequence_1[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es.foldRight[Either[E, List[A]]](Right(List.empty[A])) {
      (a, b) =>
        for {
          aa <- a
          bb <- b
        } yield aa :: bb
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as.foldRight[Either[E, List[B]]](Right(List.empty[B])) {
        (a, b) => f(a).flatMap(aa => b.map(bb => aa :: bb))
      }

    def traverse_1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as.foldRight[Either[E, List[B]]](Right(List.empty[B])) {
        (a, b) =>
          for {
            aa <- f(a)
            bb <- b
          } yield aa :: bb
      }
  }

  def main(args: Array[String]): Unit = {
    import Either._

    assert(sequence[String, Int](List(Right(1), Right(2), Right(3))) == Right(List(1, 2, 3)))
    assert(sequence[String, Int](List(Right(1), Left("SOME_ERROR"), Right(3))) == Left("SOME_ERROR"))

    assert(sequence_1[String, Int](List(Right(1), Right(2), Right(3))) == Right(List(1, 2, 3)))
    assert(sequence_1[String, Int](List(Right(1), Left("SOME_ERROR"), Right(3))) == Left("SOME_ERROR"))

    assert(
      traverse[String, Int, Boolean](List(1, 3, 5, 7, 9))(i => if (i % 2 == 0) Left("Even found") else Right(true)) == Right(List(true, true, true, true, true))
    )

    assert(
      traverse[String, Int, Boolean](List(1, 2, 5, 7, 9))(i => if (i % 2 == 0) Left("Even found") else Right(true)) == Left("Even found")
    )

    assert(
      traverse_1[String, Int, Boolean](List(1, 3, 5, 7, 9))(i => if (i % 2 == 0) Left("Even found") else Right(true)) == Right(List(true, true, true, true, true))
    )

    assert(
      traverse_1[String, Int, Boolean](List(1, 2, 5, 7, 9))(i => if (i % 2 == 0) Left("Even found") else Right(true)) == Left("Even found")
    )

  }
}
