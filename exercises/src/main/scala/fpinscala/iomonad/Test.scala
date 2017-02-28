package fpinscala.iomonad

import fpinscala.iomonad.IO2a.Return

/**
  *
  * fpinscala exercises - 14/07/16
  * Created with ♥ in Amsterdam
  */

object Executor {
  def -- [A](exp: => A) = ()
  def -> [A](exp: => A) = exp

}
object Test extends App {

  import Executor._
  -- {IO1.PrintLine("Converter").run}
  -- {IO1.converter.run}

  --  {IO1.PrintLine("Echo").run}
  -- {IO1.echo.run}

  -- {IO1.PrintLine("10 lines").run}

-- {
    (
      for {
        lines <- IO1.lines
        _ <- IO1.PrintLine(lines.toString())
      } yield ()
    ).run
  }
  // equivalent to:
  //  IO1.lines.flatMap(list => IO1.PrintLine(list.toString())).run

  // this gets a SOF exception!
  -- {IO1.IO.replicateM(10000)(IO1.PrintLine("hello!"))}

  -- { IO1.factorialREPL.run }
  //SOF
  -- { IO1.IO.forever(IO1.PrintLine("ciao")).run }


  -- {IO2a.run(IO2a.IO.replicateM(10000)(IO2a.printLine("hello!"))) }
  -> {IO2a.run(IO2a.IO.forever(IO2a.printLine("hello!"))) }

  //use trampolines
  //SOF
  val f = (x: Int) => x
  val g = List.fill(100000)(f).foldLeft(f)(_ compose _)
  -- { g(42) }

  //Alternative with IO:
  val f1: Int => IO2a.IO[Int] = (x: Int) => IO2a.Return(x)

  val g1 = Stream.fill(100000)(f1).foldLeft(f1) {
    (a, b) => x => IO2a.Suspend(() => ()).flatMap { _ => a(x).flatMap(b) }
  }
  -> {
        IO2a.run{
          for {
            out <- g1(42)
            _ <- IO2a.printLine(out.toString)
          }
            yield ()
        }
      }


}
