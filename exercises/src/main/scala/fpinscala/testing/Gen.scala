package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/
//
//trait Prop {
//  //check should communicate either a failure with description of the failure + nr of successes before failure, or a nr of successes
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//}

sealed trait Result {
  def isFalsified: Boolean
}

//this is a singleton
case object Passed extends Result {
  def isFalsified = false
}

//this is not a singleton, just a case class
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  //TODO: see how to express the failure in left/right using tag
  def && (p: Prop) = Prop((maxSize, n, rng) => run(maxSize, n, rng) match {
    case Passed => p.run(maxSize, n, rng)
    case Falsified(f, s) => Falsified(f, s)
  })


  def || (p: Prop) = Prop((maxSize, n, rng) => run(maxSize, n, rng) match {
    case Passed => Passed
    case Falsified(f, s) => p.run(maxSize, n, rng)
  })

  //given the property, enrich the representation of the falsified case
  def tag(msg: String) = Prop((maxSize, n, rng) => run(maxSize, n, rng) match {
    case Passed => Passed
    case Falsified(f, s) => Falsified(msg + "\n" + f, s)
  })


}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](sgen: SGen[A])(f: A => Boolean): Prop = ???

  //construct a property, ready for run
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop( (maxSize, n, rng) =>
    randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, iteration) => try {
        if(f(a)) Passed
        else Falsified(a.toString, iteration)
      } catch {
        case exception:Exception => Falsified(buildMsg(a, exception), iteration)
      }
    } //Stream of Results
    .find(_.isFalsified).getOrElse(Passed)
  )



  //stream of values coming from the generator g, with rng as source for randomness
  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  //build message when having exception on run with value s
  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}


//SGen needs a size and creates a generator for that max size
case class SGen[+A](forSize: Int => Gen[A])  {

  def map[B](f: A => B) = SGen(n => forSize(n).map(f))

  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(n => forSize(n).flatMap(f))

}

case class Gen[A] (sample: State[RNG, A]){

  //converts a Gen to a sized Gen, just from the point of view of the definition
  def unsized: SGen[A] = SGen(_ => this)

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  //Ex 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

//  def listOfN(size: Gen[Int]): Gen[List[A]] = Gen(for {
//    sampledSize <- size.sample
//    list <- Gen.listOfN(sampledSize, this)
//  } yield list)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap(n => Gen.listOfN(n, this))


}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))
  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeLessThan(2)).map(_ % 2 == 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(0).map(_ => g.sample)))
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State[RNG, Int](RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))

  //Ex 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap(coin => if(coin) g1 else g2)

  //Ex 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Gen(State(RNG.double)).flatMap(x => if(x < g1._2 / (g1._2 + g2._2)) g1._1 else g2._1)

  //Ex 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(listOfN(_, g))
}

object TestState extends App {
  val rng = RNG.Simple(123123)

  val myUnit = Gen.unit(5)
  println(myUnit.sample.run(rng))

  val myBoolean = Gen.boolean
  val (b, next) = myBoolean.sample.run(rng)
  println(b, next)
  println(myBoolean.sample.run(next))

  val myListOf10 = Gen.listOfN(10, Gen.boolean)
  println(myListOf10.sample.run(next))
}

