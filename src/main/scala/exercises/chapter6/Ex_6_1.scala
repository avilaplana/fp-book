package exercises.chapter6

object Ex_6_1 {

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
      case (i,r) if i > Int.MinValue => (i.abs,r)
      case (i,r) if i == Int.MinValue => nonNegativeInt(r)
    }
  }


  def main(args: Array[String]) = {
    val (i1,r1) = nonNegativeInt(new SimpleRNG(100))
    val (i2,r2) = nonNegativeInt(r1)
    val (i3,r3) = nonNegativeInt(r2)
    val (i4,r4) = nonNegativeInt(r3)

    println(i1)
    println(i2)
    println(i3)
    println(i4)
  }
}
