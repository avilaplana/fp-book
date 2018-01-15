package exercises

//Write a function size that counts the number of nodes (leaves and branches) in a tree.
object Ex_3_25 {

  sealed trait Tree [+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  object Tree {
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l,r) => size(l) + size(r)
    }
  }

  def main(args: Array[String]): Unit = {
    import Tree._
    assert(size(Leaf(1)) == 1)
    assert(size(Branch(Leaf(1), Leaf(2))) == 2)
    assert(size(Branch(Branch(Leaf(1), Leaf((2))), Branch(Leaf(1), Leaf((2))))) == 4)
  }
}
