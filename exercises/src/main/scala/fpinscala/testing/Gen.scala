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

trait Prop {
  //check should communicate either a failure with description of the failure + nr of successes before failure, or a nr of successes
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A] (sample: State[RNG, A]){

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

}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

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

