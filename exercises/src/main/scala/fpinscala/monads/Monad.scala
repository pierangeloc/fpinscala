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


  /**
    *  Ex 11.4
    */
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

  /**
    * Ex 11.7
    * Implement the Kleisli composition in terms of flatmap
    */
  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = { a =>
    flatMap(f(a))(g)
  }

  /**
    * Ex 11.8: Implement in terms of `compose`:
    * */

  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = {

    //my original solution: apply identity (except from a flatten operation in between) to the original monad ma, and then compose with f
    //    compose(unit[M[A]] andThen(flatMap(_)(unit[M[A]])), f)(ma)

    // Author's solution: Create a function that transform a unit to the monad ma, and compose it with f. The function could have been equally accepting
    // any integer and applied to an integer, e.g.:
    //    compose((_: Int) => ma, f)(1)
    compose((_: Unit) => ma, f)(())
  }

  /** Ex 11.12
    * Implemnet join, aka flatten
    **/
  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(x => x)

  /**
    *  Ex 11.13 Implement flatmap in terms of `join`:
    */
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

}



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
    * State[S, A] defines monadic operations for a given state type S, therefore we have a _family_ of monads, one for every type S
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

  /**
    * Ex 11.18
    * - The meaning of replicateM in a state monad, is like chaining M state transformations and keeping the output values in the final list
    * - map2 chains the first state transformation with the second one, and transforms the output through the given function of 2 arguments
    * - sequence will chain all the state transformations together, save the single outputs and provide them as a final output in the list
    */

  /**
    * Ex 11.19
    * getState(setState(s)) === s
    * for {
    * x <- getState
    * _ <- setState(x)
    * } yield ()
    *
    * for {
    * _ <- setState(s)
    * x <- getState
    * } yield x === s
    * when A = S, unit(s) = setState(s)
    */



  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)
  }

  def readerMonad[R] = ???
}

/**
  * Ex 11.17
  * @param value
  * @tparam A
  */
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

/**
  * Ex 11.20
  * Reader monad reads from a context and extracts a value from the context. We have like for the State monad, a family of monads, one per type of context
  * - unit reads always the same value
  * - flatMap chains 2 reads, one that extracts A, and then applies the reader function that comes from the application of f to the returned value on the first read, to the same context
  * - sequence of reader monads groups in a list the extraction of values from the initial context, that is shared across all the monads in the list
  * - replicateM returns the list of values extracted from reading from the same context n times
  * - unit law means that if I read one value out of the context and then apply anoter read that just reads the same value, it's the same as just applying 1 read
  * - associative law means that if I have to read 3 values out of a context, I can read equivalently the first 2 and then the third, or the first one and then the second and third, or equivalently, ignore the parenthesization of extraction order
  *
  */
case class Reader[R, A](run: R => A)
object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = Reader(r => f(st.run(r)).run(r))
  }
}

