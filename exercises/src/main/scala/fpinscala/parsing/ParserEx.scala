package fpinscala.parsing

//import language.{higherKinds, implicitConversions}
import ParserImp.Parser._
// import ParserOps._

object ParserEx extends App {
  val inS1 = "abbracadabbra"

  val p1 = string("abbra")
  val p2 = "cadabbra"
  val p3 = "abbra" | "cadabbra"

  val out1 = run(p1 ** p2)("abbracadabbra")
  val out2 = run(p1)("abbra")

  println("out1", out1)
  println("out2", out2)

  println {
    run("kit" | "cat")("kit")
  }
  println {
    run("kit" | "cat")("nope")
  }

  val kc = "kitcatkit"
  val kc2 = "catkitkit"

  println("slice alone", run(string("parser").slice)("parser"))

  val pOrMany = "kit" | "cat".many1
  val pOrManySlice = pOrMany.slice
  val manyEx = "kit".many1
  println("many1 or", run(pOrMany)(kc2))
  println("many1 slice or", run(pOrManySlice)(kc))
  println("many1 alone", run(manyEx)("kitkitkit"))



}
