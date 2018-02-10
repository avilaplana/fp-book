package exercises.chapter6

object Ex_6_7 {

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

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](rng => (Nil, rng)) {
      (a, b) =>
        rng =>
          val (aa, rng1): (A, RNG) = a(rng)
          val (l, rng2): (List[A], RNG) = b(rng1)
          (aa :: l, rng2)
    }

  def main(args: Array[String]) = {
    val f1: Rand[String] = r => {
      val (a, rgn) = r.nextInt
      (s"$a is String", rgn)
    }

    val f2: Rand[String] = r => {
      val (a, rgn) = r.nextInt
      (s"$a in London", rgn)
    }

    val s: Rand[List[String]] = sequence[String](List(f1, f2))
    val (l,rng) = s(new SimpleRNG(1000))
    println(l)
  }


}
