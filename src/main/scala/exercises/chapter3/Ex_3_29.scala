package exercises.chapter3

import exercises.chapter3.Ex_3_25.Tree.size
import exercises.chapter3.Ex_3_25.{Branch, Leaf, Tree}
import exercises.chapter3.Ex_3_26.{Branch, Leaf, Tree}
import exercises.chapter3.Ex_3_26.Tree.max
import exercises.chapter3.Ex_3_27.{Branch, Leaf, Tree}
import exercises.chapter3.Ex_3_27.Tree.depth
import exercises.chapter3.Ex_3_28.{Branch, Leaf, Tree}
import exercises.chapter3.Ex_3_28.Tree.map


//Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
// Reimplement them in terms of this more general function.
object Ex_3_29 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l,r) => size(l) + size(r)
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(l) => Leaf(f(l))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def depth(t: Tree[Int]): Int = t match {
      case Leaf(_) => 1
      case Branch(l,r) => scala.math.max(1 + depth(l), 1 + depth(r))
    }

    def max(t: Tree[Int]): Int = t match {
      case Leaf(l) => l
      case Branch(l,r) => scala.math.max(max(l),max(r))
    }

    def fold[A, B](t: Tree[A])(f: A => B): Int = ??? //todo
  }

  def main(args: Array[String]): Unit = {
    import Tree._

    assert(map(Leaf(1))(x => x + 1) == Leaf(2))
    assert(map(Branch(Leaf(1), Leaf(2)))(x => x + 1) == Branch(Leaf(2), Leaf(3)))

    assert(depth(Leaf(1)) == 1)
    assert(depth(Branch(Leaf(1), Leaf(2))) == 2)
    assert(depth(Branch(Branch(Leaf(1), Leaf((7))), Branch(Leaf(3), Leaf((5))))) == 3)
    assert(depth(Branch(Branch(Leaf(1), Branch(Leaf(6), Leaf((18)))), Branch(Leaf(3), Leaf((5))))) == 4)
    assert(depth(Branch(Branch(Leaf(1), Leaf((7))), Leaf(8))) == 3)

    assert(max(Leaf(1)) == 1)
    assert(max(Branch(Leaf(1), Leaf(2))) == 2)
    assert(max(Branch(Branch(Leaf(1), Leaf((7))), Branch(Leaf(3), Leaf((5))))) == 7)
    assert(max(Branch(Branch(Leaf(1), Leaf((7))), Leaf(8))) == 8)

    assert(size(Leaf(1)) == 1)
    assert(size(Branch(Leaf(1), Leaf(2))) == 2)
    assert(size(Branch(Branch(Leaf(1), Leaf((2))), Branch(Leaf(1), Leaf((2))))) == 4)
  }
}
