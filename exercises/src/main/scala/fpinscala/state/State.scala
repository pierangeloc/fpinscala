package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }


  //Ex 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
//    ((n - Int.MinValue) % (Int.MaxValue + 1), rng2)
    (if (n < 0) { n - Int.MinValue } else n, rng2)
  }

  //Ex 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = rng.nextInt
    (if (n < 0) { (n - Int.MinValue).toDouble / Int.MaxValue } else n.toDouble / Int.MaxValue, rng2)
  }

  def doubleWithMap(rng: RNG): (Double, RNG) = map(int)((n: Int) => if (n < 0) { (n - Int.MinValue).toDouble / Int.MaxValue } else n.toDouble / Int.MaxValue)(rng)

  //Ex 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = rng1.nextInt
    ( (n1, n2.toDouble / Int.MaxValue), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = rng1.nextInt
    ( (n2.toDouble / Int.MaxValue, n1), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = rng1.nextInt
    val (n3, rng3) = rng2.nextInt
    ( (n1.toDouble / Int.MaxValue, n2.toDouble / Int.MaxValue, n3.toDouble / Int.MaxValue), rng3)
  }


  //generate a list of <count> random integers
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count <= 0) (Nil, rng)
    else {
      val (n1, rng1) = rng.nextInt
      val (tail, finalRng) = ints(count - 1)(rng1)
      ((n1 :: tail), finalRng)
    }
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(acc: List[Int], rng1: RNG, n: Int): (List[Int], RNG) = {
      if (n == 0) (acc, rng1)
      else {
        val (nextNr, rng2) = rng1.nextInt
        go(nextNr :: acc, rng2, n - 1)
      }
    }

    go(Nil, rng, count)
  }

  //define a type alias for a function from RNG to (A, RNG) that generalizes what we've been doing so far
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

    //Ex 6.6
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = (rng: RNG) => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))
  def randIntDouble: Rand[(Int, Double)] = both(int, double)
  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  //Ex 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight( unit(List[A]()) )((a: Rand[A], tail: Rand[List[A]]) => map2(a, tail)((h: A, t: List[A]) => h :: t) )
//    fs.foldRight( unit(List[A]()) )((a, tail) => map2(a, tail)(_ :: _))
  }

  def intsWithSequence(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  def nonNegativeLessThan(n: Int): Rand[Int] = map(nonNegativeInt)(_ % n)

  //Ex 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThanWithFlatMap(n: Int): Rand[Int] = flatMap(nonNegativeInt)((i: Int) =>{
    val mod = i % n
    if(i + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThanWithFlatMap(n)
  })

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(f(a, _)))


  }


//Ex 6.10 (i)
/**
 * State class represents a state transition, i.e. a function from state to value and new state
 * @param run
 * @tparam S
 * @tparam A
 */
case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State( s => {
    val (v, nextState) = this.run(s)
    (f(v), nextState)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State( s => {
    val (v1, s1) = this.run(s)
    val (v2, s2) = sb.run(s1)
    (f(v1, v2), s2)
  })


  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (v1, s1) = this.run(s)
    f(v1).run(s1)
  })

  //to set state to a particular value, remaining in the contract of an output being in shape of State[S, X]
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  //to get the state, a transition from the state s to (s, s) will expose the internal state
  def get[S](): State[S, S] = State(s => (s, s))

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def mutate(machine: Machine, input: Input): Machine = {
    (machine, input) match {
      case (Machine(true, candies, coins), Coin) if candies > 0 => Machine(locked = false, candies, coins + 1)
      case (Machine(false, candies, coins), Turn) => Machine(locked = true, candies - 1, coins)

      case (machine1, _) => machine1

    }
  }

  type Rand[A] = State[RNG, A]

  // return state of machine and number of candies/coins
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State( (machine: Machine) => {
      val finalMachine = inputs.foldLeft(machine)((m, input) => mutate(m, input))
      ((finalMachine.candies, finalMachine.coins), finalMachine)
    }
  )

  //Ex 6.10 (ii)
  def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight(State.unit[S, List[A]](Nil))((a: State[S, A], b: State[S, List[A]]) => a.map2(b)((head: A, tail: List[A]) => head :: tail))

}


object TestState extends App {
  val rng = RNG.Simple(123123)
  val (next, nextRng) = rng.nextInt
  println((next, nextRng))
  val (next2, nextRng2) = nextRng.nextInt
  println((next2, nextRng2))

  //TODO: add more tests
}