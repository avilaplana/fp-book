package exercises.chapter3

//Write a function map, analogous to the method of the same name on List,
// that modifies each element in a tree with a given function.
object Ex_3_28 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(l) => Leaf(f(l))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def main(args: Array[String]): Unit = {
    import Tree._
    assert(map(Leaf(1))(x => x + 1) == Leaf(2))
    assert(map(Branch(Leaf(1), Leaf(2)))(x => x + 1) == Branch(Leaf(2), Leaf(3)))
  }
}
