package other

object Sky {

  def substrings(s: String): List[String] = s.toList match {
    case Nil => Nil
    case h :: Nil => List(h.toString)
    case h :: tail => substrings(tail.toString()).map(m => (h :: m.toList).toString())

  }

  def main(args: Array[String]) = {

    println(substrings("abb"))
//    assert(substrings("abb") == List("abb", "ab", "bb", "a", "b"))

  }

}
