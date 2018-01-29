package exercises.chapter4

import com.sun.javaws.exceptions.InvalidArgumentException

//Implement the variance function in terms of flatMap. If the mean of a sequence is m, the variance is the mean of
// math.pow(x - m, 2) for each element x in the sequence
object Ex_4_2 {

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

    def get: A = this match {
      case None => throw new RuntimeException("there is no value")
      case Some(v) => v
    }
  }


  object Option {
    def variance(xs: Seq[Double]): Option[Double] = {

      val mean: Seq[Double] => Option[Double] = s => if (s.isEmpty) None else Some(s.sum / s.length)
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }
  }


  case object None extends Option[Nothing]

  case class Some[A](value: A) extends Option[A] {

  }

  def main(args: Array[String]): Unit = {
    import Option._

    def truncate(d: Double): Double = (math floor d * 100) / 100

    assert(truncate(variance(Seq(1, 4, 5, 7, 8, 10)).get) == 8.47D)
    assert(truncate(variance(Seq(1, 2, 5, 9, 10, 12)).get) == 16.91D)
    assert(variance(Nil) == None)
  }
}
