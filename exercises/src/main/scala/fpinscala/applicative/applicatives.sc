import fpinscala.applicative
import fpinscala.applicative.Applicative

val F: Applicative[Option] = new Applicative[Option] {

  override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = for {
    a <- fa
    b <- fb
  } yield f(a, b)

  override def unit[A](a: => A): Option[A] = Some(a)
}

val depts: Map[String, String] = Map("Alice" -> "Management", "Bob" -> "Cleaning", "Charlie" -> "Accounting")
val salaries: Map[String, Int] = Map("Alice" -> 500000, "Bob" -> 10000, "Charlie" -> 200000)
val  o = F.map2(depts.get("Alice"), salaries.get("Alice")){
  (dept, salary) => s"wateva"
}

/**
  * Ex 12.4: stream applicative
  * Sequencing streams means taking a list of streams and producing a stream that
  * creates tuples made of one element picked from each of the sequenced streams.
  * It' like parallelizing the production of elements from streams, or transposing the matrix (whose length or height is can be not limited)
  */

import Applicative.streamApplicative
val s1 = Stream.from(0)
val s2 = Stream.from(1000)
val s3 = Stream.continually("Constant")

streamApplicative.sequence(List(s1, s2, s3)).take(4).toList

