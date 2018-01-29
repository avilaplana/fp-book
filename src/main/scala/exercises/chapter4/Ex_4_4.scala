package exercises.chapter4

import scala.annotation.tailrec

//Write a function sequence that combines a list of Options into one Option containing a list of all the Some values in the original list.
// If the original list contains None even once, the result of the function should be None; otherwise the result should be Some with a list
// of all the values
object Ex_4_4 {

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

    /*
 Here's an explicit recursive version:
 */
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a match {
        case Nil => Some(Nil)
        case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
      }
    /*
    It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
    Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
    unfortunate consequence of Scala using subtyping to encode algebraic data types.
    */
    def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  }

  def main(args: Array[String]): Unit = {
    import Option._

    assert(sequence(List(Some(1), Some(2), Some(3), Some(4))) == Some(Seq(1, 2, 3, 4)))
    assert(sequence(List(Some(1), None, Some(3), Some(4))) == None)

    assert(sequence_1(List(Some(1), Some(2), Some(3), Some(4))) == Some(Seq(1, 2, 3, 4)))
    assert(sequence_1(List(Some(1), None, Some(3), Some(4))) == None)
  }
}
