package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //Ex 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }


  //Ex 5.2a
  final def take(n: Int): Stream[A] = this match {
    // the cool thing about the smart constructor is that it builds a Cons where we don't even evaluate the function h, nor t.
    // It just instructs the Cons object how to populate the lazy parameters the first time we need them
    case Cons(h, t) if (n > 0) => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  //Ex 5.2b
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n == 0) => t()
    case Cons(h, t) => t().drop(n - 1)
    case _ => Empty
  }

  //Ex 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def exists2(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if(p(h()) || t().exists(p)) => true
    case _ => false
  }

  //Examples of how to use foldRight
  //Ex 5.4
  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  ///Ex 5.5
  def takeWhile2(p: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A])((a, b) => if( p(a)) cons(a, b) else Empty)

  //Ex 5.6 (hard???)
  def headOption2: Option[A] = this.foldRight(None: Option[A])((a, b) => (Some(a)))

  //Ex 5.7
  def map[B](f: A => B): Stream[B] = this.foldRight(Stream.empty: Stream[B])((h, t) => cons(f(h), t))

  def append[B >: A](s2: Stream[B]): Stream[B] = this.foldRight(s2)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]) = this.foldRight(Stream.empty: Stream[B])((h, t) => f(h).append(t))



  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty => None
  }


  //Infinite Streams/Corecursion
  //Ex 5.13
  def mapWithUnfold[B](f: A => B) = Stream.unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def takeWithUnfold(n: Int): Stream[A] = Stream.unfold((n, this)) {
    case (k, Cons(h, t)) if (k > 0) => Some(h(), (k - 1, t()))
    case _ => None
  }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if(p(h())) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C) : Stream[C] = Stream.unfold((this, bs)) {
    case (Cons(h0, t0), Cons(h1, t1)) => Some(f(h0(), h1()), (t0(), t1()))
    case _ => None
  }

  //this continues to zip as long as at least one stream has elements
  def zipAll[B](s1: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, s1)) {
      case (Cons(h0, t0), Cons(h1, t1)) => Some( (Some(h0()), Some(h1()) ), (t0(), t1()))
      case (Cons(h0, t0), Empty) => Some( (Some(h0()): Option[A], None: Option[B]), (t0(), empty[B]))
      case (Empty, Cons(h1, t1)) => Some( (None: Option[A], Some(h1()): Option[B]), (empty[A], t1()))
      case _ => None
    }
  }


  //Ex 5.14
  def startsWith[B](s: Stream[B]): Boolean = this.zipWith(s)((_,_)).forAll({case (x, y) => x == y})

  //Ex 5.15
  def tails: Stream[Stream[A]] = ???


}

case object Empty extends Stream[Nothing]
//why h and t are real thunks and not just lazily evaluated parameters?
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // this is a smart constructor: it memoizes the value to be returned by the head,
  // so we are sure this value is cached and evaluated only when the head function is actually called
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  //corecursion/infinite streams
  val ones: Stream[Int] = Stream.cons(1, ones)

  //Ex 5.8
  def constant[A](c: A): Stream[A] = Stream.cons(c, constant(c))

  //Ex 5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  //Ex 5.10
  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = Stream.cons(a + b, go(b, a + b))
    cons(0, cons(1, go(0, 1)))
  }

  //Ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((newA, newState)) => cons(newA, unfold(newState)(f))
    case None => empty
  }

  //Ex 5.12
  def fibsWithUnfold(): Stream[Int] = unfold((0, 1)) {case(a, b) => Some(a, (b, a + b))}
  def fromWithUnfold(n: Int): Stream[Int] = unfold(n)((s: Int) => Some(s, s + 1))
  def constantWithUnfold[A](c: A): Stream[A] = unfold(c)((s: A) => Some(s, s))
  def onesWithUnfold(): Stream[Int] = unfold(1)((s: Int) => Some(s, s))

}

object TestStream {
  def main(args: Array[String]): Unit = {
    println(Stream.from(-10).take(10).toList)
    println(Stream.fromWithUnfold(-10).take(10).toList)

    println(fibs().take(10).toList)
    println(fibsWithUnfold().take(10).toList)

    println(constantWithUnfold(5).take(5).toList)

    println(Stream.from(-10).take(10).mapWithUnfold(_ + 5).toList)

    println(Stream.from(-100).takeWithUnfold(20).toList)

    val s1 = Stream.from(-100)
    val s2 = Stream.from(0).take(10)

    println(s1.zipWith(s2)((a: Int, b: Int) => a * b).toList)

    println(Stream.from(-10).take(20).zipAll(Stream.from(-100).take(10)).toList)
    println(Stream.from(-10).take(20).zipWith(Stream.from(-100))((_, _)).take(10).toList)

    println(Stream.from(0).take(20).startsWith(Stream.from(0).take(10)))

  }

}

