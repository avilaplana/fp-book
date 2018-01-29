package exercises.chapter4

//In this implementation, map2 is only able to report one error, even if both the name and the age are invalid.
//What would you need to change in order to report both errors? Would you change map2 or the signature of mkPerson?
//Or could you create a new data type that captures this requirement better than Either does, with some additional structure?
//How would orElse, traverse, and sequence behave differently for that data type?
object Ex_4_8 {

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
    //todo

  }
}
