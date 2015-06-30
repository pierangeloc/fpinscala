package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
/**
 * A Monoid must provide an associative binary operation, and a neutral element
 * @tparam A
 */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2

    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2

    def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2

    def zero = true
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2

    def zero = false
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = {
      //return first option if not None, else the second one
      map2(a1, a2)((x, y) => x) orElse (a2)
      //much simpler to just do:
      a1 orElse a2
    }

    def zero: Option[A] = None
  }

  //let's reimplement the map2 for convenience
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap ((valA: A) => b map ((valB: B) => f(valA, valB)))


  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f1: A => A, f2: A => A): A => A = f1 andThen f2

    //zero is the identity function
    def zero = (a: A) => a
  }

    import fpinscala.testing._

    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = Prop.forAll(Gen.listOfN(3, gen).map {
      case a :: b :: c :: Nil => (a, b, c)
    }) {
      case (x, y, z) => m.op(m.op(x, y), z) == m.op(x, m.op(y, z))
    } &&
      Prop.forAll(gen) {
        case x => m.op(m.zero, x) == m.op(x, m.zero)
      }

    //given a list and a monoid on A, fold them through the operation defined in A
    def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
    def foldMapRight[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldRight(m.zero)((a, b) => m.op(f(a), b))

    //foldRight in terms of foldMap
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = ???

    //foldLeft in terms of foldMap
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = ???

    //version of foldMap that splits the sequence in subsequences, and applies the folding separately to the chunks
    def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      as match {
        case Nil => m.zero
        case (a1: A) :: Nil => f(a1)
        case _ => {
          val (l, r) = as.splitAt(as.length)
          m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
        }
      }
    }

    def ordered(ints: IndexedSeq[Int]): Boolean =
      sys.error("todo")

    sealed trait WC

    case class Stub(chars: String) extends WC

    case class Part(lStub: String, words: Int, rStub: String) extends WC

    def par[A](m: Monoid[A]): Monoid[Par[A]] =
      sys.error("todo")

    def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
      sys.error("todo")

    val wcMonoid: Monoid[WC] = sys.error("todo")

    def count(s: String): Int = sys.error("todo")

    def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
      sys.error("todo")

    def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
      sys.error("todo")

    def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
      sys.error("todo")

    def bag[A](as: IndexedSeq[A]): Map[A, Int] =
      sys.error("todo")

}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}


