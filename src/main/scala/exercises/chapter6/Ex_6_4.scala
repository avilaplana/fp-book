package exercises.chapter6

import exercises.chapter6.Ex_6_3.SimpleRNG
import exercises.chapter6.Ex_6_4.RNG

//Write a function to generate a Double between 0 and 1, not including 1.
// Note: You can use Int.MaxValue to obtain the maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double.
object Ex_6_4 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL * 0XBL) & 0XFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    count match {
      case 0 => (Nil, rng)
      case n =>
        val (i, r1) = rng.nextInt
        val (l, r2) = ints(n - 1)(r1)
        (i :: l, r2)
    }
  }


  def main(args: Array[String]) = {
    val (l1, r1) = ints(5)(new SimpleRNG(1000))
    val (l2, r2) = ints(5)(r1)
    println(l1)
    println(l2)
  }

  //  val (l1, r1) = ints(5)(new SimpleRNG(1000))

  //    count = 5
  //  val (i5, r5) = rng.nextInt
  //  val (l5, r4) = ints(4)(r5)
  //  (i5 :: l5, r4)
  //    count = 4
  //  val (i4, r4) = r5.nextInt
  //  val (l4, r3) = ints(3)(r4)
  //  (i4 :: l4, r3)
  //    count = 3
  //  val (i3, r3) = r4.nextInt
  //  val (l3, r2) = ints(2)(r3)
  //  (i3 :: l3, r2)
  //    count = 2
  //  val (i2, r2) = r3.nextInt
  //  val (l2, r1) = ints(1)(r2)
  //  (i2 :: l2, r1)
  //    count = 1
  //  val (i1, r1) = r2.nextInt
  //  val (l1, r0) = ints(0)(r1)
  //  (i1 :: l1, r0)
  //    count = 0
  //  (Nil,r1)

//  ((i1 :: Nil), r1)
//  ((i2:: (i1 :: Nil)), r1)
//  ((i3 :: (i2:: (i1 :: Nil))), r1)
//  ((i4 :: (i3 :: (i2:: (i1 :: Nil)))), r1)
//  ((i5 :: (i4 :: (i3 :: (i2:: (i1 :: Nil))))), r1)


}
