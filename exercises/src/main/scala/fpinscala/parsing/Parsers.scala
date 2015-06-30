package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parser[A] {

}

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def run[A](p: Parser[A])(s: String): Either[ParseError, A]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]


  def char(c: Char): Parser[Char]

  def many[A](p: Parser[A]): Parser[List[A]]


  // count occurrences of a character, on a String starting with it
  def counter(c: Char): Parser[Int] = map(many(char(c)))(_.size)

  // count if one or more occurrences, fail if string not starting with 'c'
  def countOneOrMore(c: Char): Parser[Int]

  //implicit conversion, gives a parser whenever it expects a Parser but finds a String
  implicit def string(s: String): Parser[String]

  //WTF???
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  //converts a type A to the equivalent String parser, given a function that gives the String representation of A
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  //parser operations on a parser
  case class ParserOps[A](p: Parser[A]) {
    //this way we define infix operators (they take 1 parameter) in term of prefix operator "or" defined in Parsers trait
    def | [B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or [B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B) = self.map(p)(f)
    def many = self.many(p)

  }

  object Laws {


    run("alpha")("alphabeta") == Right("alpha")

    run("alpha" | "beta")("alphabeta") == Right("alpha")
    run("alpha" | "beta")("betaalpha") == Right("beta")

    run(listOfN(3, "ab" | "cad"))("abcadcad") == Right(List("ab", "cad", "cad"))
    run(listOfN(3, "ab" | "cad"))("ababab") = Right(List("ab", "ab", "ab"))
    run(listOfN(3, "ab" | "cad"))("cadabab") == Right(List("cad", "ab", "ab"))



  }
}

object Me extends App {
  val p: Parsers[Parser[String]] = null
  import p._
  "abra" | "cadabra"

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