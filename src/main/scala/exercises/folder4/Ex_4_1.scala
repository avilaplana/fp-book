package exercises.folder4


//Implement all of the preceding functions on Option. As you implement each function
object Ex_4_1 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def orElse[B >: A](default: => Option[B]): Option[B] = this match {
      case None => default
      case Some(v) => Some(v)
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case None => None
      case Some(v) if f(v) => Some(v)
      case _ => None
    }
  }

  case object None extends Option[Nothing]

  case class Some[A](value: A) extends Option[A] {

  }

  def main(args: Array[String]): Unit = {

    assert(Some(1).map(x => x + 1) == Some(2))
    assert(None.map((x: Int) => x + 1) == None)

    assert(Some("alvaro").flatMap(x => Some(x.toUpperCase)) == Some("ALVARO"))
    assert(None.flatMap((x: String) => Some(x.toUpperCase)) == None)

    assert(Some("alvaro").getOrElse("mark") == "alvaro")
    assert(None.getOrElse("mark") == "mark")

    assert(Some("alvaro").orElse(Some("mark")) == Some("alvaro"))
    assert(None.orElse(Some("mark")) == Some("mark"))

    assert(Some("alvaro").filter(x => x.startsWith("alv")) == Some("alvaro"))
    assert(Some("mark").filter(x => x.startsWith("alv")) == None)
    assert(None.filter((x:String) => x.startsWith("alv")) == None)
  }
}
