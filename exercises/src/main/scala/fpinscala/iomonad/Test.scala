package fpinscala.iomonad

/**
  *
  * fpinscala exercises - 14/07/16
  * Created with ♥ in Amsterdam
  */
object Test extends App {
  IO1.PrintLine("Converter").run
  IO1.converter.run

  IO1.PrintLine("Echo").run
  IO1.echo.run

//  IO1.PrintLine("10 lines").run
//  (
//    for {
//      lines <- IO1.lines
//      _ <- IO1.PrintLine(lines.toString())
//    } yield ()
//  ).run
  // equivalent to:
  //  IO1.lines.flatMap(list => IO1.PrintLine(list.toString())).run

  // this gets a SOF exception!
  // IO1.IO.replicateM(10000)(IO1.PrintLine("hello!")).run

  IO1.factorialREPL.run
  IO1.IO.forever(IO1.PrintLine("ciao")).run
}
