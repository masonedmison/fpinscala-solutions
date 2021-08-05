package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case object Proved extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount, message: String="Test Failed") extends Result {
  def isFalsified = true
  }

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (m, n, s) => {
      (run(10,n, s), p.run(10, n, s)) match {
        case (Passed|Proved, Passed|Proved) => Passed
        case (_, Falsified(f, sc, _)) => Falsified(f, sc, "right failed")
        case (Falsified(f, sc, _), _) => Falsified(f, sc, "left failed")
      }
    }
  }
  def ||(p: Prop): Prop = Prop {
    (m, n, s) => {
      (run(m, n,s), p.run(m, n, s)) match {
        case (Passed|Proved, _) => Passed
        case (_, Passed|Proved) => Passed
        case (Falsified(f1, sc1, _), Falsified(_, _, _)) => Falsified(f1, sc1, "left and right failed")
      }
    }
  }
}

object Prop {
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  val sortedListProp = {
    val initG = Gen.choose(0, 100)
    forAll(listOf1(initG)) {
      l =>
        val ls = l.sorted
        ls.zip(ls.slice(1, ls.size) ++ List(Int.MaxValue)).map {
          case (x, y) => x < y
        }.exists(_ != false)
    }
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
    
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = {
    Prop {
      (max, n, rng) =>
        val casesPerSize = (n - 1) / max + 1
        val props: Stream[Prop] = {
          Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        }
        val prop: Prop =
          props.map(p => Prop { (max, n, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)
        prop.run(max, n, rng)
    }
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )
  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n, _) =>
        println(s"! Falsified after $n passed tests: \n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests")
      case Proved =>
        println(s"+ OK, proved property")
    }

  val pint = {
    choose(1, 10) map Par.unit
  }

  val forkProp = {
    forAllPar(pint) { n =>
      val x = Par.unit(n)
      Par.equal(x, es => Par.fork(x)(es))
    }
  }

  val nested2Pint = {
    choose(1, 10).map(x => Par.flatMap(Par.unit(x))(Par.unit))
  }
}
case class Gen[A](sample: State[RNG, A]) {

  def apply(s: State[RNG, A]) = Gen(s)

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap(x => this.listOfN(x))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(x => f(x).sample))
  }

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }
  
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def unsized: SGen[A] = {
    SGen {
      i => this

    }
  }
  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean(w: Double): Gen[Boolean] =
    Gen(State(RNG.boolean(w)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    weighted((g1, .5), (g2, .5))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g1._2.abs)
    boolean(threshold).flatMap(b => if(b) g1._1; else g2._1)
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen {
      n => g.listOfN(unit(n + 1))
    }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen {
      n => g.listOfN(unit(n))
    }
}

case class SGen[A] (g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen { n => apply(n) ** s2(n) }
}

object ** {
  def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)
}
