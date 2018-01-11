package exercises

object Ex_2_3 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

  def main(args: Array[String]): Unit = {

    assert(curry((a: Int, b: Int) => a + b)(1)(2) == 3)

    assert(curry((a: Int, b: Int) => a + b)(2)(3) == 5)

  }

}
