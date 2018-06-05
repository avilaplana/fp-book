package exercises.chapter7

object Ex_7_1 {

    trait Par[A]

    object Par {

        def unit[A](a: => A): Par[A] = ???

        def get[A](a: Par[A]): A = ???

        def map2[A](f1: => Par[A], f2: => Par[A])(f3: (A, A) => A): Par[A] = ???
    }

    def sum(ints: IndexedSeq[Int]): Par[Int] =
        if (ints.size <= 1)
            Par.unit(ints.headOption getOrElse 0)
        else {
            val (l, r) = ints.splitAt(ints.length / 2)
            Par.map2(sum(l), sum(r))(_ + _)
        }
}
