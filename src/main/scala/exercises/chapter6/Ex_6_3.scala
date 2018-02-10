package exercises.chapter6

//Write a function to generate a Double between 0 and 1, not including 1.
// Note: You can use Int.MaxValue to obtain the maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double.
object Ex_6_3 {

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
      case (i, r) if i == Int.MaxValue => double(r)
      case (i, r) => (i.abs.toDouble / Int.MaxValue, r)
    }
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def main(args: Array[String]) = {
    val ((i1, d1), r1) = intDouble(new SimpleRNG(1000))
    val ((i2, d2), r2) = intDouble(r1)

    val ((d3, i3), r3) = doubleInt(r2)
    val ((d4, i4), r4) = doubleInt(r3)

    val ((d5, d6, d7), r5) = double3(r4)
    val ((d8, d9, d10), r6) = double3(r5)


    println((i1, d1))
    println((i2, d2))
    println((d3, i3))
    println((d4, i4))
    println((d5, d6, d7))
    println((d8, d9, d10))
  }
}
