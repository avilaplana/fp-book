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
}
