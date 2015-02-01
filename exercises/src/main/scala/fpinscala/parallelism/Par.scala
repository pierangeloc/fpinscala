package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value.
  // It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  // Fork is a way to mark a parallel computation for explicit parallelism (forking)
  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting
  // for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`,
  // this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      //      def call = a(es).get
      def call = run(es)(a).get
    })


  //N.B. Scala has no implements, so we are extending an interface and take care of implementing all its methods
  //N.B. The fact that the input parameter is called get, defines a get implicitly, that implements the get method required by Future interface,
  //     hence no need to implement it again explicitly
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API
  // for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of
      // the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation
      // that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
      UnitFuture(f(af.get, bf.get))
    }


  def map2WithGoodTimeouts[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      private case class UnitFutureWithTimeout[A](get: A, timeout: Long, units: TimeUnit) extends Future[A] {
        override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???
        override def isCancelled: Boolean = ???
        override def get(timeout: Long, unit: TimeUnit): A = ???
        override def isDone: Boolean = ???
      }
      // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of
      // the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation
      // that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
      UnitFuture(f(af.get, bf.get))

    }

  //Ex 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => {
    map2(unit(f(a)), unit(()))((a, b) => a)
  }
  def asyncF2[A, B](f: A => B): A => Par[B] = (a: A) => fork(unit(f(a)))



  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
    //list of par[B]
    sequence(ps.map(asyncF(f)))

    //combine list of par to a par of list!!!
  }

  //Ex 7.5
  //usual process we've already seen in previous chapters. This isn't actually parallelizing much
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(Nil: List[A]))((p, z) => map2(p, z)(_ :: _))
  }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}