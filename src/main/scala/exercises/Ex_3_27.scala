package exercises

//Write a function depth that returns the maximum path length from the root of a tree to any leaf.
object Ex_3_27 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def depth(t: Tree[Int]): Int = ???
  }

  def main(args: Array[String]): Unit = {
    import Tree._
    assert(depth(Leaf(1)) == 1)
    assert(depth(Branch(Leaf(1), Leaf(2))) == 2)
    assert(depth(Branch(Branch(Leaf(1), Leaf((7))), Branch(Leaf(3), Leaf((5))))) == 3)
    assert(depth(Branch(Branch(Leaf(1), Branch(Leaf(6), Leaf((18)))), Branch(Leaf(3), Leaf((5))))) == 4)
    assert(depth(Branch(Branch(Leaf(1), Leaf((7))), Leaf(8))) == 2)
  }
}
