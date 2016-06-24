package fpinscala
package applicative

import monads.Functor
import state._
import State._
import fpinscala.testing.Prop
import jdk.nashorn.internal.runtime.regexp.joni.constants.Traverse
//import StateUtil._ // defined at bottom of this file
import monoids._

trait Applicative[F[_]] extends Functor[F] {
  self =>

  //primitive: map2 & unit
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  //all applicatives are functors:
  def map[A,B](fa: F[A])(f: A => B): F[B] = map2(fa, unit())((a, _) => f(a))
  //    apply(unit(f))(fa)

  /**
    * Ex 12.1
    */
  def sequence[A](fas: List[F[A]]): F[List[A]] = fas.foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]()))((a, fa) => map2(f(a), fa)(_ :: _))


  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  /**
    * Ex 12.2: Applicative can be equivalently formulated in terms of unit and apply (that's why they are called _applicatives_)
    */
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))
  def map2Alt[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C) = {
    apply(apply(unit(f.curried))(fa))(fb)
    //or
    //apply(map(fa)(f.curried))(fb)
  }

  /**
    * Ex 12.3
    */
  def map3[A, B, C, D](fa: F[A],
  fb: F[B],
  fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A],
  fb: F[B],
  fc: F[C],
  fd:F[D])(f: (A, B, C, D) => E): F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  /**
    * Ex 12.8: product of 2 applicatives
    */
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    //primitive: map2 & unit
    override def map2[A, B, C](faGa: (F[A], G[A]), fbGb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = (faGa, fbGb) match {
      //want to use first map2 of Applicative[F], then map2 of Applicative[G]
      case ((fa, ga), (fb, gb)) => (self.map2(fa, fb)((a, b) => f(a, b)), G.map2(ga, gb)((a, b) => f(a, b)))
    }

    override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
  }

  /**
    * Ex 12.9
    */
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {

    def unit[A](a: =>A): F[G[A]] = self.unit(G.unit(a))

    //primitive: map2 & unit
    def map2[A, B, C](fGa: F[G[A]], fGb: F[G[B]])(f: (A, B) => C): F[G[C]] = self.map2(fGa, fGb)((gA, gB) => G.map2(gA, gB)(f))
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ???
}




/**
  * Applicative laws
  */
trait ApplicativeLaws[F[_]] extends Applicative[F] {
  /**
    * 1. Identity law
    * map2 with identity function projection should behave like the induced map function
    */
  def identityLaw[A, B](fa: F[A], fb: F[B]) = {
    map2(fa, unit())((a, _) => a) == fa
    map2(unit(), fa)((_, a) => a) == fa
  }

  /**
    * Helper transformation
    */
  def assoc[A, B, C](t: (A, (B, C))): ((A, B), C) = t match {
    case (a, (b, c)) => ((a, b), c)
  }

  /**
    * 2. Associative Law
    * combining products one way or another guarantees the same kind of result.
    * This way map3 or map4 behave the same way regardless of how we decide to group them
    */
  def associativeLaw[A, B, C](fa: F[A], fb: F[B], fc: F[C]) = {
    product(product(fa, fb), fc) == map(product(fa, product(fb, fc)))(assoc)
  }

  /**
    * Product of functions
    */
  def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) = (i, i2) => (f(i), g(i2))

  /**
    * 3. Naturality of Product
    * When combining 2 applicatives, we can apply the transformation before or after having them
    * combined, and the result is the same
    */
  def naturalityOfProduct[A, A1, B, B1](fa: F[A], fb: F[A1])(f: A => B, g: A1 => B1) = {
    map2(fa, fb)(productF(f, g)) == product(map(fa)(f), map(fb)(g))
  }
}


case class Tree[+A](head: A, tail: List[Tree[A]])


/**
  * Monad as subtype of Applicative[F]
  */
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)


  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
  // i.e.
  //  for {
  //    a <- ma
  //    b <- mb
  //  } yield f(a, b)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  /**
    * Ex 12.5
    */
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = {
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = ma.right.flatMap(f)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

//  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
//    Monad[({type f[x] = F[N[x]]})#f] = ???
}

/**
  * If compose validation through Either, being map2/3 implemented in terms of flatMap, it gives up as soon as one validation fails
  * We must instead re-implement this validation to e.g. accumulate errors and return them at the end
  */
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  /**
    * Ex 12.6
    */
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =  new Applicative[({type f[x] = Validation[E,x]})#f] {

    def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, hb +: (tb ++ ta))
      case (fa@Failure(_, _), _) => fa
      case (_, fb@Failure(_, _)) => fb
      case (Success(a), Success(b)) => Success(f(a, b))
    }

    def unit[A](a: => A): Validation[E, A] = Success(a)
  }



  //  type Const[A, B] = A
//
//  implicit def monoidApplicative[M](M: Monoid[M]) =
//    new Applicative[({ type f[x] = Const[M, x] })#f] {
//      def unit[A](a: => A): M = M.zero
//      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
//    }
}
//
//trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
//  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
//    sequence(map(fa)(f))
//  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
//    traverse(fma)(ma => ma)
//
//  type Id[A] = A
//  val idMonad = new Monad[Id] {
//    def unit[A](a: => A) = a
//    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
//  }
//
//  def map[A,B](fa: F[A])(f: A => B): F[B] =
//    traverse[Id, A, B](fa)(f)(idMonad)
//
//  import Applicative._
//
//  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
//    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
//      as)(f)(monoidApplicative(mb))
//
//  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
//    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)
//
//  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
//    traverseS(fa)((a: A) => (for {
//      s1 <- get[S]
//      (b, s2) = f(a, s)
//      _  <- set(s2)
//    } yield b)).run(s)
//
//  override def toList[A](fa: F[A]): List[A] =
//    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse
//
//  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
//    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1
//
//  def reverse[A](fa: F[A]): F[A] = ???
//
//  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = ???
//
//  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
//                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???
//
//  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
//}
//
//object Traverse {
//  val listTraverse = ???
//
//  val optionTraverse = ???
//
//  val treeTraverse = ???
//}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
//// them here
//object StateUtil {
//
//  def get[S]: State[S, S] =
//    State(s => (s, s))
//
//  def set[S](s: S): State[S, Unit] =
//    State(_ => ((), s))
//}
