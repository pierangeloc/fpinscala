package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter


sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  //Ex 4.1
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)
//  = this match {
//    case Some(x) => f(x)
//    case None => None
//  }

  //todo: impl without patmat?
  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)
//  this match {
//    case None => ob
//    case _ => _
//  }

  //todo: impl without patmat?
  def filter(f: A => Boolean): Option[A] = flatMap((x:A) => if (f(x)) Some(x) else None)
//    this match {
//    case Some(x) if f(x) => Some(x)
//    case _ => None
//  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  import math.pow
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //Ex 4.2
  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap ((m: Double) => mean( xs map ((x: Double) => pow(x - m, 2))))

  //Ex 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap( (aa: A) => b map (f(aa, _)))

  //Ex 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
//    a.foldRight[Option[List[A]]](Some(Nil))((optA: Option[A], optListA: Option[List[A]]) => optListA flatMap( list => (optA map (_ :: list))) )
    a.foldRight[Option[List[A]]](Some(Nil))(( optA, optListA) => map2(optA, optListA)(_ :: _))


  //Ex 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))(( elemA, optListB) => map2(f(elemA), optListB)(_ :: _))

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)((x: Option[A]) => x)


  def Try[A](a: =>A): Option[A] = try {
    Some(a)
  } catch {
    case e: Exception => None
  }
}

object TestOption {
  def main(args: Array[String]): Unit = {
    val values = List(1.0, 2.0, 3.0, 4.0,5.0)
    println("mean: " + Option.mean(values))
    println("squared errors:" + (values map ((x: Double) => Math.pow(x - 3, 2))) )
    println("Testing variance on Sequence: " + values + ": var = " + Option.variance(values))
    println("Testing sequence / 1 : " + Option.sequence(List(Some(1), Some(2), Some(3))))
    println("Testing sequence / 2 : " + Option.sequence(List(Some(1), Some(2), None)))

    import Option.Try
    //example of a list of string that fail to be parsed all together to a list of ints
    def parseIntOpt: String => Option[Int] = (s: String) => Try(s.toInt)
    println(List("something", "1", "2", "3").map(parseIntOpt(_)))
    println(Option.traverse(List("something", "1", "2", "3"))(parseIntOpt))
    println(Option.traverse(List("99", "1", "2", "3"))(parseIntOpt))


  }

}