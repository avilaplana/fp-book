package exercises

//Implement dropWhile, which removes elements from
// the List prefix as long as they match a predicate.
object Ex_3_5 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case list => list
    }


    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    import List.dropWhile
    assert(dropWhile[Int](Nil, e => true) == Nil)
    assert(dropWhile[Int](List(1, 2, 3), _ <= 2) == List(3))
    assert(dropWhile[String](List("al", "alvaro", "putney", "london"), (e: String) => e.startsWith("al")) == List("putney", "london"))
  }


}
