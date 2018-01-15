package exercises.chapter2

object Ex_2_4 {

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def main(args: Array[String]): Unit = {

    assert(uncurry((a: Int) => (b: Int) => a + b)(1,2) == 3)

    assert(uncurry((a: Int) => (b: Int) => a + b)(2,3) == 5)

  }

}
