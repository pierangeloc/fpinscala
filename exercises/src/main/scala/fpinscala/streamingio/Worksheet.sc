import fpinscala.streamingio.SimpleStreamTransducers._

val p = Process.liftOne((x: Int) => x * 10)
p(Stream(1,2,3)).toList
p.repeat(Stream(1,2,3)).toList //this acts as a map

Process.sum(Stream((1 to 10).map(_.toDouble).toList: _*)).toList

Process.take(5)(Stream.from(5)).toList