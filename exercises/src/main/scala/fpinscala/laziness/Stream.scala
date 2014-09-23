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

  //Ex 5.4
  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a: A, b: Boolean) => p(a) && b)

  ///Ex 5.5
  def takeWhile2(p: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A])((a: A, b: Stream[A]) => if( p(a)) cons(a, b) else Empty)

  //Ex 5.6 (hard???)
  def headOption2: Option[A] = this.foldRight(None: Option[A])((a: A, b: => Option[A]) => (Some(a)))

  //Ex 5.7
  def map[B](f: A => B): Stream[B] = this.foldRight(Stream.empty: Stream[B])((h, t) => cons(f(h), t))

  def append[B >: A](s2: Stream[B]): Stream[B] = this.foldRight(s2)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]) = this.foldRight(Stream.empty: Stream[B])((h, t) => f(h).append(t))

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty => None
  }
}

case object Empty extends Stream[Nothing]
//why h and t are real thunks and not just lazily evaluated parameters?
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}