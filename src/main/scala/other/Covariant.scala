package other

object Covariant {

  sealed trait Animal {
    val name: String
  }
  case class Bird(name: String) extends Animal
  case class Dog(name: String) extends Animal

  class Container[A](a: A) {
  }

  class ContainerCovariant[+A](a: A) {
  }


  def main(args: Array[String]) = {

    val animals: Seq[Animal] = Seq(Bird("bird1"), Dog("dog1"))
    val dogs: Seq[Animal] = Seq(Dog("dog1"), Dog("dog2"))
    val birds: Seq[Animal] = Seq(Bird("bird1"), Bird("bird2"))
    val noAnimales: Seq[Animal] = Nil

    //val container: Container[Animal] = new Container[Bird](Bird("bird")) does not compile
    val containerCovariant: ContainerCovariant[Animal] = new ContainerCovariant[Bird](Bird("bird"))

  }

}
