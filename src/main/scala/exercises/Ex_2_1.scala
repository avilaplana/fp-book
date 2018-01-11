package exercises

import scala.annotation.tailrec

//Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
//The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the previous two—the sequence begins 0, 1, 1, 2, 3, 5.
//Your definition should use a local tail-recursive function.

object Ex_2_1 {

  def fib(n: Int): Int = {
    @tailrec
    def calc(element: Int, pre1: Int, pre2: Int): Int = if (element == n) pre1 + pre2 else calc(element + 1, pre2, pre1 + pre2)

    if (n == 1) 0
    else if (n == 2) 1
    else calc(3, 0, 1)
  }

  def main(args: Array[String]): Unit = {

    assert(fib(1) == 0)

    assert(fib(2) == 1)

    assert(fib(3) == 1)

    assert(fib(4) == 2)

    assert(fib(5) == 3)

    assert(fib(6) == 5)
  }

}
