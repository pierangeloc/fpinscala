package fpinscala
package monads

import fpinscala.parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  /** Distribute a functor of product type into a product of functor**/
  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  /** Distribute a sum (coproduct) type of functor into a functor of coproducts **/
  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }

  /**
   *
   *    scala> val pairsList = List((1,"a"), (2,"b"), (3, "c"))
   *            pairsList: List[(Int, String)] = List((1,a), (2,b), (3,c))
   *
   *  distribute does the same as unzip
   *    scala> listFunctor.distribute(pairsList)
   *    res1: (List[Int], List[String]) = (List(1, 2, 3),List(a, b, c))
   */
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(f(a, _)))

  /**
    *  Ex 11.3
    */
  def sequence[A](lma: List[M[A]]): M[List[A]] = lma.foldRight(unit(List[A]()))((el, tail) => map2(el, tail)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = la.foldRight(unit(List[B]()))((el, tail) => map2(f(el), tail)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  /**
    * Ex 11.6 (real brainer stuff)
    * - we make it recursive, empty case is trivial
    * - non empty case:
    * -- the starting point is if the predicate is true, therefore we need to flatmap or map f(x)
    * -- considering we have recursion, we must flatmap otherwise we have to deal with M[M[M...
    * -- if condition is false, we just return the filter on the tail
    * -- if condition is true, we filter the tail, and map this monad with the append operator to the current tail
    *
    */
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    ms match {
      case List() => unit(List())
      case x :: xs => {
        flatMap(f(x)) { condition =>
          if (!condition) filterM(xs)(f)
          else map(filterM(xs)(f))(x :: _)
        }
      }
    }
  }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = ???

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???

  def join[A](mma: M[M[A]]): M[A] = ???

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  /**
    * Ex 11.1
    */
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)
    def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma.flatMap(f)

  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  /**
    * State[S, A] defines monadic operations for a given type S, therefore we have a _family_ of monads, one for every type S
    * So, we need to parameterize the monad on S
    * One way is to include code in a class parameterized on S
    * Another way is to define a structural type and access its type projection
    *
    * @tparam S
    * @return
    */
  def stateMonad[S] = new Monad[({type s[X] = State[S, X]})#s] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] = sa.flatMap(f)
  }

  val idMonad: Monad[Id] = ???

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???
  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

