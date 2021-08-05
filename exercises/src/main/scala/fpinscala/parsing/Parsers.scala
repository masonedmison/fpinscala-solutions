package fpinscala.parsing

// import fpinscala.parsing.JSON.{JBool, JNumber, JObject, JString}

import language.{higherKinds, implicitConversions}
import fpinscala.testing._

import scala.util.matching.Regex

trait Result[+A] {

  def uncommit: Result[A] = this match {
    case Failure(e, true) => Failure(e, isCommited = false)
    case _ => this
  }
  def mapError(f: ParseError => ParseError): Result[A] =
    this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e, c) => Failure(e, c || isCommitted)
    case _ => this
  }

  def advanceSuccesses(n: Int): Result[A] = this match {
    case Success(a, m) => Success(a, n+m)
    case _ => this
  }

}

object Result {
  import ParserImp._

  def attempt[A](p: MyParser[A]): MyParser[A] =
    s => p(s).uncommit
}

case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError, isCommited: Boolean) extends Result[Nothing]

object ParserImp {
  type MyParser[+A] = Location => Result[A]

  object Parser extends Parsers[MyParser] {
    def or[A](s1: MyParser[A], s2: => MyParser[A]): MyParser[A] =
      loc =>
        s1(loc) match {
          case Failure(_, false) => s2(loc)
          case r => r
        }

    implicit def string(s: String): MyParser[String] = {
      scope(s"Parsing failed for $s")(loc => {
        if (loc.input.slice(loc.offset, loc.input.length).startsWith(s)) {
          Success(s, loc.offset + s.length)
        }
        // TODO is this the correct flag for isCommited?
        else Failure(loc.toError(s"expected $s but found ${loc.input}"), isCommited = false)
        }
        )
    }

    // TODO should this only consider regex at beginning of string??
    // TODO check updating of Success chars consumed...
    implicit def regex(r: Regex): MyParser[String] =
      loc =>
        r.findFirstIn(loc.input) match {
          case Some(x) => Success(x, loc.offset + x.length)
         // TODO is this the correct flag for isCommited?
          case _ => Failure(loc.toError(s"Expected pattern of ${r.toString}"), isCommited=true)
        }

    def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = {
      p(Location(input)) match {
        case Success(a, _) => Right(a)
        case Failure(e, _) => Left(e)
      }
    }

    def slice[A](p: MyParser[A]): MyParser[String] =
      loc =>
        p(loc) match {
          case Success(_, c) => Success(loc.input.slice(loc.offset, c), c)
          case e @ Failure(_, _) => e
        }

    def flatMap[A, B](f: MyParser[A])(g: A => MyParser[B]): MyParser[B] =
      s => f(s) match {
        case Success(a, n) => g(a)(s.advanceBy(n))
                                .addCommit(n != 0)
                                .advanceSuccesses(n)
        case e @ Failure(_, _) => e
      }

    def label[A](msg: String)(p: MyParser[A]): MyParser[A] =
      s => p(s).mapError(_.label(msg))

    def errorLocation(e: ParseError): Location = ???

    // Shouldn't this return an option?
    def errorMessage(e: ParseError): String =
      e.stack.headOption match {
        case Some(x) => x._2
        case None => "No Error messages to report!"
      }

    def scope[A](msg: String)(p: MyParser[A]): MyParser[A] =
      loc => p(loc).mapError(_.push(loc, msg))

    def succeed[A](a: A): MyParser[A] =
      loc =>
        Success(a, loc.offset)
  }

}

trait Parsers[Parser[+_]] {

  self => // so inner classes may call methods of trait

  import JSON._

  // type Parser[A] = String => Either[ParseError, ResState[A]]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)
  }

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p)) { case (x, y) => x :: y }

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(f andThen succeed)

  def map2ViaProduct[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    product(p, p2).map { case (x, y) => f(x, y) }
  }

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    map2(p, p2)((_, _))

  def slice[A](p: Parser[A]): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def skipL[A, B](l: Parser[A], r: Parser[B]): Parser[B] =
    map2(l, r)((_, r) => r)

  def skipR[A, B](l: Parser[A], r: Parser[B]): Parser[A] =
    map2(l, r)((l, _) => l)

  val skipWhiteSpace: Parser[String] = {
    char(' ').many.slice
  }

  val parseJString: Parser[JSON] = {
    for {
      _ <- skipWhiteSpace
      _ <- char('"')
      s <- "[A-z]+".r.map(JString)
      _ <- char('"')
    } yield s
  }

  val parseJNumber: Parser[JSON] = """[0-9]+""".r.map(x => JNumber(x.toDouble))

  val parseJArray: Parser[JSON] = {
      skipL(char('['), skipR(
        parseJString | parseJNumber,
        char(',') | char(']')).many1
      ).map(x => JArray(x.toVector))
  }

  // TODO needs work
  def parseJObject: Parser[JSON] = ???
    // v2
/*
    JObject(for {
      k <- skipL(char('{'), skipR(parseJString, char(':')))
      v <- parseJString | parseJNumber
    } yield Map(k.asInstanceOf[JString].get -> v))
*/


  val numFollChar: Parser[String] = {
    flatMap("[0-9]+".r)(a => listOfN(a.toInt, "a").map(_.mkString))
  }

  def succeed[A](a: A): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many1: Parser[List[A]] = self.many1(p)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def slice: Parser[String] = self.slice(p)
  }
  object Laws {

    import Prop._

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => self.run(p1)(s) == self.run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map((a: A) => a))(in)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        self.run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }
      }
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line: Int = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List()
                      ) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label(s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption
}


trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  // def jsonParser[Err, Parser[+_]](p: Parsers[Err, Parser]): Parser[JSON] = ???

}