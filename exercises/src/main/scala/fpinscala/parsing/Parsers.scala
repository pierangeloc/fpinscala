package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parser[A] {
}

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait


  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  //apply parser p and then apply the one returned by f on the rest, and return the parsed value
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def run[A](p: Parser[A])(s: String): Either[ParseError, A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  /**
   * Attempts the parsers ???
   */
  def attempt[A](p: Parser[A]): Parser[A]

  def opt[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) or succeed(None)

  /**
   * Provide parser with an error message to be used in case of failure
   */
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  /**
   * Define a new parser that applies first p1, then p2 and combines their extracted results with a combining function f that returns a C
   */
  def map2[A, B, C](p1: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] = ???

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  /**
    * Return the string processed by p when successful, else return an empty string. Primitive
   */
  def slice[A](p: Parser[A]): Parser[String]

  /**
   * apply p many times and return list of matches
   */
  def many[A](p: Parser[A]): Parser[List[A]]


  //Ex 9.3
  def manyFromMap2[A](p: Parser[A]): Parser[List[A]] = map2(p, manyFromMap2(p)) {case(a, listOfA) => a :: listOfA} or succeed(Nil)

  //Ex 9.4
  def listOfNFromMap2[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n <= 0) succeed(Nil) else map2(p, listOfNFromMap2(n - 1, p))(_ :: _)

  /**
   * Apply p 1 or more times, and return list of matches
   *
   */
  def many1[A](p: Parser[A]): Parser[List[A]] = (p ** many(p)).map {case (a, la) => a :: la}

  /**
   * apply 1 parser and then another one and produce pair of results
   */
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = map2(p1, p2)((_, _))

  /**
   * count occurrences of a character, can return zero
   */
  def counter(c: Char): Parser[Int] = char(c).many.slice.map(_.size)

  /**
   *   count at least one occurrence of a character, returns error when no such char found
   */
  def countOneOrMore(c: Char): Parser[Int]

  def startsWithAndFollow(a: Char, b: Char): Parser[(SuccessCount, SuccessCount)] = map2(counter(a), countOneOrMore(b))((_,_))

  /**
   * Ex 9.6: parse a String starting with a digit n and continuing with n repetition of character c
   * @param c
   */
  def parseNChar(c: Char) = "[0-9]".r.flatMap(n => listOfN(n.toInt, char(c)))

  /**
   * Ex 9.7
   */
  def productViaFlatMap[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] = p1.flatMap(a => p2.map(b => (a, b)))
  def map2ViaFlatMap[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C) = p1.flatMap(a => p2.map(b => f(a, b)))

  /**
   * Ex 9.8
   */
  def mapViaFlatMap[A, B](p: Parser[A])(f: A => B) = p.flatMap(a => succeed(f(a)))

  /**
   * Consume one or more digits
   */
  def digits: Parser[String] = "\\d+".r

  /**
   * Consume a string that represents a double
   * @return
   */
  def doubleString: Parser[String] = "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?".r

  /**
   * Sequence 2 parsers and ignore the result of the first one
   */
  def skipL[A](pLeft: Parser[Any], p: Parser[A]): Parser[A] = map2(slice(pLeft), p)((l, r) => r)

  /**
   * Sequence 2 parsers, ignoring the result of the second one
   */
  def skipR[A](p: Parser[A], pRight: Parser[Any]) = map2(p, slice(pRight))((l, r) => l)

  def token(t: String): Parser[String] = string(t)

  /**
   * Parser which consumes one or more whitespaces
   */
  def whiteSpaces: Parser[String] = "\\s*".r


  /**
   * Parse and recognize any String that matches the provided regex
   */
  implicit def regex(r: Regex): Parser[String]

  /**
   *   implicit conversion, gives a parser whenever it expects a Parser but finds a String
   */
  implicit def string(s: String): Parser[String]

  /**
   *   implicit conversion from parser to ParserOps
   */
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  /**
   *   converts a type A to the equivalent String parser, given a function that gives the String representation of A
   */
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  /**
   *   parser operations on a parser
   */
  case class ParserOps[A](p: Parser[A]) {
    //this way we define infix operators (they take 1 parameter) in term of prefix operator "or" defined in Parsers trait
    def | [B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or [B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B) = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
    def many = self.many(p)
    def slice = self.slice(p)
    def product[B](p2: Parser[B]) = self.product(p, p2)
    def **[B](p2: Parser[B]) = product(p2)
    def *>[B](p2: Parser[B]) = self.skipL(p, p2)
    def <*(p2: Parser[Any]) = self.skipR(p, p2)


  }

  object Laws {
//    import Prop._
    val check1: Prop = check(run("alpha")("alphabeta") == Right("alpha"))

    val orProperty: Prop = check(run("alpha" | "beta")("alphabeta") == Right("alpha"))
    val orCommutes = check(run("alpha" | "beta")("betaalpha") == Right("beta"))


    val listOfN1 = check(run(listOfN(3, "ab" | "cad"))("abcadcad") == Right(List("ab", "cad", "cad")))
    val listOfN2 = check(run(listOfN(3, "ab" | "cad"))("ababab") == Right(List("ab", "ab", "ab")))
    val listOfN3 = check(run(listOfN(3, "ab" | "cad"))("cadabab") == Right(List("cad", "ab", "ab")))

    val charParserGenerator = Gen.choose(0, 26).map(n => char(('a' + n).toChar))
    //empty string should always match any input string
    def emptyStringParserAlwaysSucceed(str: Gen[String]) = forAll(str)((s: String) => run(string(""))(s) == Right(""))

    def equal[A](p1: Parser[A], p2: Parser[A])(str: Gen[String]): Prop  = forAll(str)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]) = equal(p, p.map(x => x))(in)
    def succeedLaw[A](a: A)(in: Gen[String]) = forAll(in)(s => run(succeed(a))(s) == Right(a))

    def productLaw[A](p1: Parser[A], succeedVal: A)(in: Gen[String]) = forAll(in){ s => {
      val succeedParser = succeed(succeedVal)
      run((succeedParser ** p1).map {case (a1, a2) => a1})(s) == Right(succeedVal)

      }
    }

    //Ex 9.2
    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

    def productIsAlmostAssociative[A](p1: Parser[A], p2: Parser[A], p3: Parser[A])(gs: Gen[String]) = equal((p1 ** (p2 ** p3)).map(unbiasR), ((p1 ** p2) ** p3).map(unbiasL))(gs)
    def productAndMap[A,B,C,D](p1: Parser[A], f: A => B, p2: Parser[C], g: C => D)(gs: Gen[String]) = equal(p1.map(f) ** p2.map(g) , (p1 ** p2) map {case (x, y) => (f(x), g(y))})(gs)

  }
}

object Me extends App {
//  val p: Parsers[Parser[String]] = null
//  import p._
//  "abra" | "cadabra"

}



case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}