package fpinscala.state

import fpinscala.state.State.unit


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def boolean(w: Double): Rand[Boolean] =
    map(double)(x => if (x < w) true; else false)

  def boolean: Rand[Boolean] =
    boolean(.5)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, rngN) = rng.nextInt
    (if (a < 0) -(a + 1) else a, rngN)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double(rng: RNG): (Double, RNG) = {
    val (a, rngN) = rng.nextInt
    ((a / Int.MaxValue).toDouble, rngN)
  }

  // also as val

  val _double2: Rand[Double] = map(_.nextInt)(n => (n / Int.MaxValue).toDouble)

  def double2: Rand[Double] = {
    map[Int, Double](_.nextInt)(n => (n / Int.MaxValue).toDouble)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (a, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((a, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (a, rng2) = rng1.nextInt
    ((d, a), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0) (Nil, rng)
    else {
      val (a, rngN) = rng.nextInt
      val (l, rngN2) = ints(count - 1)(rngN)
      (a :: l, rngN2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def ints2(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(_.nextInt))
  }


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => fs.foldRight((Nil: List[A], rng))((a, s) => {
      val (nA, nS) = a(s._2)
      (nA :: s._1, nS)
    })

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, s) = f(rng)
      g(a)(s)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(g: A => B): Rand[B] =
    flatMap(s) {
      x => unit(g(x))
    }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(g: (A, B) => C): Rand[C] =
    flatMap(ra) {
      x =>
        y =>
          val (a, s) = rb(y)
          unit(g(x, a))(s)
    }
}


case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap {
      x =>
        State.unit(f(x))
    }

  def map2Reg[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State {
      (st: S) => {
        val (a, s1) = run(st)
        val (b, s2) = sb.run(s1)
        (f(a, b), s2)
      }
    }
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap {
      x => sb.map(b => f(x, b))
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State {
      st =>
        val (a, s) = run(st)
        f(a).run(s)
    }
  }
}


sealed trait Input {
  def inputToState: State[Machine, (Int, Int)] =
    State {
      (mach: Machine) =>
        if (mach.candies == 0) {
          ((mach.candies, mach.coins), mach)
        }
        else {
          this match {
            case Turn =>
              if (!mach.locked)
                ((mach.candies - 1, mach.coins), Machine(locked = true, mach.candies - 1, mach.coins))
              else
                ((mach.candies, mach.coins), Machine(mach.locked, mach.candies, mach.coins))

            case Coin =>
              if (!mach.locked)
                ((mach.candies, mach.coins), Machine(mach.locked, mach.candies, mach.coins))
              // unlocked
              else
                ((mach.candies, mach.coins + 1), Machine(locked = false, mach.candies, mach.coins + 1))
          }
        }
    }
}

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  // possible to generate a Gen[Option[A]] from Gen[A] and vice versa?
  // recall Gen[A] is a wrapper for a State of [RNG, A] and is accessed like g.sample...
  // how can we lift sample to be an option?


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequenceViaFoldRight(inputs.map(_.inputToState)).map(_.last)

  def unit[S, B](b: B): State[S, B] =
    State {
      (s: S) => (b, s)
    }

  def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
  }
}

object Candy {
  def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(((f: Machine => Machine) => modify[Machine](f)).compose(update)))
    s <- get
  } yield (s.coins, s.candies)

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}