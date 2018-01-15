package exercises.chapter3

//See what happens when you pass Nil and Cons themselves to foldRight, like this:
//foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
//What do you think this says about the relationship between foldRight and the data constructors of List?

object Ex_3_8 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List._

    assert(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))

    //    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    //    Cons(1, foldRight(Cons(2, Cons(3,Nil), Nil)(Cons(_,_))
    //    Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil)(Cons(_,_))
    //    Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil)(Cons(_,_))
    //    Cons(1, Cons(2, Cons(3, Nil)))

  }
}
