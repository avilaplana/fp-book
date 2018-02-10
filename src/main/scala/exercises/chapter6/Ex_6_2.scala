package exercises.chapter6

import exercises.chapter6.Ex_6_1.{SimpleRNG, nonNegativeInt}

//Write a function to generate a Double between 0 and 1, not including 1.
// Note: You can use Int.MaxValue to obtain the maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double.
object Ex_6_2 {

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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, r) if i >= 0 => (i, r)
      case (i, r) if i == Int.MinValue => nonNegativeInt(r)
      case (i, r) => (-i, r)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    rng.nextInt match {
      case (i, r) if i == Int.MaxValue => (0D, r)
      case (i, r) => (i.abs.toDouble / Int.MaxValue, r)
    }

  }


  def main(args: Array[String]) = {
    val (i1, r1) = double(new SimpleRNG(1000))
    val (i2, r2) = double(r1)
    val (i3, r3) = double(r2)
    val (i4, r4) = double(r3)

    println(i1)
    println(i2)
    println(i3)
    println(i4)
  }
}
