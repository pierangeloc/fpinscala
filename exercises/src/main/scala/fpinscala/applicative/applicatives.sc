import java.text.SimpleDateFormat
import java.util.Date

import fpinscala.applicative.{Applicative, Failure, Success, Validation}
import fpinscala.state.State

val F: Applicative[Option] = new Applicative[Option] {

  override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = for {
    a <- fa
    b <- fb
  } yield f(a, b)

  override def unit[A](a: => A): Option[A] = Some(a)
}

val depts: Map[String, String] = Map("Alice" -> "Management", "Bob" -> "Cleaning", "Charlie" -> "Accounting")
val salaries: Map[String, Int] = Map("Alice" -> 500000, "Bob" -> 10000, "Charlie" -> 200000)
val  o = F.map2(depts.get("Alice"), salaries.get("Alice")){
  (dept, salary) => s"wateva"
}

/**
  * Ex 12.4: stream applicative
  * Sequencing streams means taking a list of streams and producing a stream that
  * creates tuples made of one element picked from each of the sequenced streams.
  * It' like parallelizing the production of elements from streams, or transposing the matrix (whose length or height is can be not limited)
  */

import Applicative.streamApplicative
val s1 = Stream.from(0)
val s2 = Stream.from(1000)
val s3 = Stream.continually("Constant")

streamApplicative.sequence(List(s1, s2, s3)).take(4).toList

//form validation with Validation applicative
case class WebForm(name: String, birthdate: Date, phoneNumber: String)
//We want:
// -  name.length > 0,
// -  birthdate format = 'yyyy-MM-dd'
// -  phoneNumber.length = 10

def validName(name: String): Validation[String, String] = {
  if (name.length > 0) Success(name)
  else Failure("Name is empty", Vector())
}

def validBirthdate(birthdate: String): Validation[String, Date] = {
  try {
    Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
  } catch {
    case _: Throwable => Failure("Date should match yyyy-MM-dd", Vector())
  }
}

def validPhoneNumber(phone: String): Validation[String, String] = {
  if(phone.matches("[0-9]{10}")) Success(phone)
  else Failure("Phone number should be 10 digits", Vector())
}

import fpinscala.applicative.Applicative.validationApplicative
def validateWebForm(name: String, birthdate: String, phoneNumber: String) = validationApplicative.map3(
  validName(name),
  validBirthdate(birthdate),
  validPhoneNumber(phoneNumber)
)(WebForm(_,_,_))

validateWebForm("R. Feynman", "1918-05-11", "0123456789")
validateWebForm("", "1918-05-11", "0123456789")
validateWebForm("", "19181231231-0weqwe5-11", "0123456789")
validateWebForm("R. Feynman", "1918-05-11", "0123456789012")

//traverse
import fpinscala.applicative.Traverse._
def zipListWithIndex[A](xs: List[A]): List[(Int, A)] = listTraverse.traverseS(xs){
  a => for {
    i <- State.get[Int]()
    _ <- State.set[Int](i + 1)
  } yield (i, a)
}.run(0)._1

zipListWithIndex(List("a", "b", "c", "d"))

