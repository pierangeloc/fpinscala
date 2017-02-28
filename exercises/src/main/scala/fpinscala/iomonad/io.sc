import fpinscala.iomonad.IO1._
import fpinscala.iomonad.Monad

//this overflows
//IO.forever(PrintLine("Ciao!")).run

val id = (x: Int) => x
val deep = List.fill(100000)(id).foldLeft(id)(_ compose _)
//this also overflows stack
//deep(666)

//wrap the output into a data type for which we provide an interpreter
sealed trait TailRec[A] { self =>
  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(self, f)
  def map[B](f: A => B): TailRec[B] = flatMap(a => Return(f(a)))
}

object TailRec extends Monad[TailRec] {
  def unit[A](a: => A): TailRec[A] = Return(a)
  def flatMap[A, B](ta: TailRec[A])(f: A => TailRec[B]): TailRec[B] = FlatMap(ta, f)

  def run[A](tailRec: TailRec[A]): A = tailRec match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => println("return"); run(f(a))
      case Suspend(r) => println("suspend");  run(f(r()))
      case FlatMap(y, g) => println("flatMap"); run(y flatMap(a => g(a).flatMap(f)))
    }
  }
}

case class Return[A](a: A) extends TailRec[A]
case class Suspend[A](resume: () => A) extends TailRec[A]
case class FlatMap[A, B](sub: TailRec[A], f: A => TailRec[B]) extends TailRec[B]

val idTailRec: Int => TailRec[Int] =  x => Return(x)
val deepTailRec = List.fill(4)(idTailRec).foldLeft(idTailRec) {
  (tr1, tr2) => x => Return(()).flatMap(_  => tr1(x).flatMap(tr2))
}
TailRec.run(deepTailRec(666))