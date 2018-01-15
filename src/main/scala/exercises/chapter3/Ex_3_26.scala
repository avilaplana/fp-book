package exercises.chapter3

//Write a function maximum that returns the maximum element in a Tree[Int].
// (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)
object Ex_3_26 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  object Tree {
    def max(t: Tree[Int]): Int = t match {
        case Leaf(l) => l
        case Branch(l,r) => scala.math.max(max(l),max(r))
    }
  }

  def main(args: Array[String]): Unit = {
    import Tree._
    assert(max(Leaf(1)) == 1)
    assert(max(Branch(Leaf(1), Leaf(2))) == 2)
    assert(max(Branch(Branch(Leaf(1), Leaf((7))), Branch(Leaf(3), Leaf((5))))) == 7)
    assert(max(Branch(Branch(Leaf(1), Leaf((7))), Leaf(8))) == 8)
  }
}
