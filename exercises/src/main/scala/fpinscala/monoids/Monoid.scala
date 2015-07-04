package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps

// infix syntax for `Par.map`, `Par.flatMap`, etc
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

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a: A, b: A) = m.op(b, a)

    def zero = m.zero
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

  //foldRight in terms of foldMap (SUPER HARD!!!)
  //we map each element of as to a function B=>B, then we compose them all together via endomonoid, and apply them to the given z
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried)(z)

  //foldLeft in terms of foldMap
  //if we fold a list of endomorphisms through a dual endomonoid, we get the application concatenated of the fn(fn-1(fn-2...)
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, dual(endoMonoid[B]))((a: A) => (b: B) => f(b, a))(z)

  //version of foldMap that splits the sequence in subsequences, and applies the folding separately to the chunks
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(as.isEmpty)  m.zero
    else if(as.size == 1) f(as(0))
    else {
        val (l, r) = as.splitAt(as.length / 2)
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
      }
  }

  //Ex 10.9
  def ordered(ints: IndexedSeq[Int]): Boolean = {

    val monoid = new Monoid[(Option[Int], Boolean)] {
      val zero = (None, true)
      def op(a1: (Option[Int], Boolean), a2: (Option[Int], Boolean)): (Option[Int], Boolean) =
          (a1, a2) match {
            case ((None, _), a2Value) => a2Value // implement Zero law
            case (a1Value, (None, _)) => a1Value // commutative
            case ((_, false), (_,_)) => (Some(Int.MinValue), false)
            case ((_,_), (_, false)) => (Some(Int.MinValue), false)
            case ((Some(a1Val), true), (Some(a2Val), true)) if (a1Val <= a2Val) => (Some(a1Val), true)
            case (_,_) => (Some(Int.MinValue), false)
           }
    }

    foldMapV(ints, monoid)((a: Int) => (Some(a), true)) match {
      case (_, isOrdered) => isOrdered
    }
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = ???

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = ???

  def wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = ???

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = ???

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = ???

}

/**
 * Trait for any foldable structure, on which we can apply the fold functions
 * F is any higher-kinded type that is defined in term of another (unknown at the moment) type
 * @tparam F
 */
trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

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
    ???

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    ???

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    ???
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    ???

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}


