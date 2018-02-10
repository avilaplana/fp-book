package exercises.chapter6

import exercises.chapter6.Ex_6_2.{RNG, SimpleRNG, double}

//Use map to reimplement double in a more elegant way
object Ex_6_5 {

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

  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def double(rng: RNG): (Double, RNG) = map(_.nextInt)(a => if (a == Int.MaxValue) 0D else a.abs.toDouble / Int.MaxValue)(rng)

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
