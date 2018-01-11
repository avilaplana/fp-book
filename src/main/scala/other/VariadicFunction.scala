package other

object VariadicFunction {


  def variadic(address: String*): Unit = {
    address match {
      case _ =>  println(s"is a : ${address.getClass}") // scala.collection.mutable.WrappedArray$
    }
    val add: Seq[String] = address

    add match {
      case _ =>  println(s"is a : ${add.getClass}") // scala.collection.mutable.WrappedArray$
    }
  }

  def main(args: Array[String]) = {

    variadic("ad1", "ad2", "ad3", "ad4")

  }
}
