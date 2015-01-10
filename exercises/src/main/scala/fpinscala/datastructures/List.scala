package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
                                      // N.B. As List is covariant it makes perfectly sense to have the empty list to extend List[Nothing]
                                      //      in this way it is a subtype of any List[B], and it can be assigned to it
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  // Ex 3.2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(_, tail) => tail
      case Nil => Nil
    }

  // Ex 3.3
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Cons(_, tail) => Cons(h, tail)
      case Nil => Nil
    }

  // Ex 3.4
  def drop[A](l: List[A], n: Int): List[A] = if (n == 0) {
    l
  } else {
    l match {
      case Cons(_, tail) => drop(tail, n - 1)
      case Nil => Nil
    }
  }


  // Ex 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, tail) if (f(x)) => tail
    case _ => l
  }

  //Ex 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, tail) => Cons(x, init(tail))
  }


  def initTailRec[A](l: List[A]): List[A] = {

    //kind of a cheating, use ListBuffer which is not purely functional, to implement it in a tail recursive way
    import scala.collection.mutable.ListBuffer
    @tailrec
    def go(lb: ListBuffer[A], l: List[A]): List[A] = {
      l match {
        case Nil => List(lb.toList :_*)
        case Cons(_, Nil) => List(lb.toList :_*)
        case Cons(x, tail) => go(lb:+ x , tail)
      }
    }

    go(new ListBuffer(), l)
  }

  //Ex 3.7
  // It is not possible to have shortcuts with foldRight, just because foldRight required going through the list till the end, and then starting evaluation
  //keep in mind that foldRight can be interpreted just replacing the Cons and Nil in the construction of the list, with f and z respectively, regardless
  // of the nature of f. Therefore no way to use short circuits here

  //Ex 3.8
  def foldRightWithCons[A] = (as: List[A]) => foldRight(as, Nil: List[A])(Cons(_,_))

  //Ex 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, n) => n + 1)

  //Ex 3.10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, tail) => foldLeft(tail, f(z, x))(f)
  }

  //Ex 3.11
  def sum3(l: List[Int]) = foldLeft(l, 0)((x, y) => x + y)
  def prod3(l: List[Int]) = foldLeft(l, 1)((x, y) => x * y)

  //Ex 3.12
  def reverse[A](l: List[A]) = foldLeft(l, Nil: List[A])((xs: List[A], x:A) => Cons(x, xs))

  //Ex 3.14
  def append2[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))
  def append3[A](l1: List[A], l2: List[A]): List[A] = foldLeft(reverse(l1), l2)((b: List[A], a: A) => Cons(a, b))


  //Ex 3.15
  def concat[A](listOfLists: List[List[A]]): List[A] = foldLeft(listOfLists, Nil: List[A])(append)

  //Ex 3.16
  def add1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, tail) => Cons(x + 1, add1(tail))
  }

  //Ex 3.17
  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, tail) => Cons(x.toString, doubleToString(tail))
  }

  def map[A, B](as: List[A])(f:A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, tail) => Cons(f(x), map(tail)(f))
  }

  def mapWithFoldRight[A, B](as: List[A])(f:A => B): List[B] =
    foldRight(as, Nil: List[B])((a: A, b: List[B]) => Cons(f(a), b))

  //todo: write a tailrecursive version of map

  //Ex 3.19
  def filter[A](as: List[A])(p: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, tail) if p(x) => Cons(x, filter(tail)(p))
    case Cons(x, tail) if !p(x) => filter(tail)(p)
  }


  //Ex 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil: List[B])((listb, a) => append(listb, f(a)))

  def flatMap1[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  //todo: write tailrec flatmap

  //Ex 3.21
  def filterWithFlatMap[A](as: List[A])(p: A => Boolean): List[A] =
    flatMap(as)((x: A) => if(p(x)) List(x) else Nil)

  //Ex 3.22
  def addOneToOne(as: List[Int], bs: List[Int]) : List[Int] =
    (as, bs) match {
      case (Cons(a, tailA), Cons(b, tailB)) => Cons(a + b, addOneToOne(tailA, tailB))
      case _ => Nil
    }


  //Ex 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C) : List[C] =
    (as, bs) match {
      case (Cons(a, tailA), Cons(b, tailB)) => Cons(f(a, b), zipWith(tailA, tailB)(f))
      case _ => Nil
    }
}

object Test {
  def main(args: Array[String]) {
    println("x from pattern matching: I expect 3: " + List.x)

    val first10 = List(1,2,3,4,5,6,7,8,9,10)
//    val first4000 = List(1 to 40000: _*)

    println("first10: " + first10)
    println("tail of first10: " + List.tail(first10))
    println("tail of Nil: " + List.tail(Nil))

    println("first10 with new head: " + List.setHead(first10, 1000))
    println("Nil with new head: " + List.setHead(Nil, 1000))

    println("first10 with no end:" + List.init(first10))
    println("first10 with no end tailrec:" + List.initTailRec(first10))

    println("foldRight with cons: " + List.foldRightWithCons(first10))

    println("length of first10:" + List.length(first10))
    println("length of Nil:" + List.length(Nil))

    println("prod3 of List(3,4,5): " + List.prod3(List(3,4,5)))
    println("sum3 of List(3,4,5): " + List.sum3(List(3,4,5)))

    println("reverse of first10: " + List.reverse(first10))

    println("append2 on twice first10: " + List.append2(first10, first10))
    println("append3 on twice first10: " + List.append3(first10, first10))

    val test = List(List(1,2,3,4), List(5,6,7,8), List(9, 10, 11, 12))
    println("concat them all: " + List.concat(test))

    println("flatmap: " + List.flatMap(List(1, 2, 3))((x: Int) => List(x, x, x, x)))
    println("flatmap1: " + List.flatMap1(List(1, 2, 3))((x: Int) => List(x, x, x, x)))

    println("add one to one: " + List.addOneToOne(List(1, 2, 3, 4, 5), List(10, 10, 10, 10)))



  }
}