package exercises.chapter2

object Ex_2_5 {

  def compose[A,B,C](f: B => C, g: A => B): A => C = (a:A) => f(g(a))

  def main(args: Array[String]): Unit = {

    assert(compose((b:Int) => b + 1, (a:Int) => a + 1)(1) == 3)

    assert(compose((b:Int) => b + 1, (a:Int) => a + 2)(1) == 4)
  }

}
