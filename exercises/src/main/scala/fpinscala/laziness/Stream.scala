package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(x, y) => x() :: y().toList
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    this match {
      case _ if `n` <= 0 => Empty
      case Cons(x, xs) => Cons(x, () => xs() take n - 1)
    }

  def take2(n: Int): Stream[A] =
    // unfold[A, Option[A]](headOption)(_ => Some(headOption, headOption))
    unfold((this, n)) {
      case (st, curI) =>
        st match {
          case Cons(x, xs) if curI > 0 => Some(x(), (xs(), curI - 1))
          case _ => None
        }
    }

  def drop(n: Int): Stream[A] =
    this match {case _ if `n` <= 0 => this
      case Cons(_, xs) => xs() drop n - 1
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(x, xs) =>
        if (p(x())) Cons(x, () => xs() takeWhile p)
        else Empty
    }

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold[A, Stream[A]](this) {
      case Cons(x, xs) if p(x()) => Some(x(), xs())
      case _ => None
    }


  def zipAll[B >: A](other: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, other)) {
      case (Cons(x, xs), Cons(y, ys)) => Some((Some(x()), Some(y())), (xs(), ys()))
      case (Empty, Cons(y, ys)) => Some((None, Some(y())), (Empty, ys()))
      case (Cons(x, xs), Empty) => Some((None, Some(x())), (xs(), Empty))
      // case (Empty, Empty) => Some((None, None), (Empty, Empty))
      case (Empty, Empty) => None
    }

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, other)) {
      case (Empty, _ ) => None
      case (_, Empty) => None
      case (Cons(x, xs), Cons(y, ys)) => Some(f(x(), y()), (xs(), ys()))

    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty: Stream[A])((x, y) => {
      if (p(x)) cons(x, y)
      else Empty
    })

  def forAll(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }

  def headOption: Option[A] =
    foldRight(None: Option[A])((x, _) => Some(x))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((x, y) => cons(f(x), y))

  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(x, xs) => Some(f(x()), xs())
      case _ => None
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((x, y) => f(x))

  // this will terminate early, yeah?
  //actually no since we return y which invariably recursively calls foldRight
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((x, y) =>
      if (p(x)) cons(x, y)
      else y)

  def append[B >: A](el: => B): Stream[B] =
    foldRight(cons(el, Empty))((x, y) => cons(x, y))

  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipWith(s)((x, y) => x == y).forAll(_ == true)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(x, xs) => Some(Cons(x, xs), xs())
      case _ => None
    }

  def scanRight[B](z: B)(op: (A, B) => B): Stream[B] =
    this match {
      case Cons(_, xs) => cons(foldRight(z)((x, y) => op(x, y)), xs().scanRight(z)(op))
      case _ => Empty
    }

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // 5.8
  def constant(n: Int): Stream[Int] = {
    Stream.cons(n, constant(n))
  }

  // 5.9
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def getNextFib(curN: Int): Int =
      curN match {
        case 0 | 1 => curN
        case _ => getNextFib(curN - 1) + getNextFib(curN - 2)
      }

    def fibAcc(n: Int): Stream[Int] = {
      lazy val curFib = getNextFib(n)
      Stream.cons(curFib, fibAcc(n + 1))
    }

    fibAcc(0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Empty
      case Some((a, s)) =>
        Stream.cons(a, unfold(s)(f))
    }
  }

  def from2(n:Int): Stream[Int] =
    unfold(n)(s => Some(s + 1, s + 1))

  def constant2(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s))

  def fib2: Stream[Int] = {
    def getNextFib(curN: Int): Option[(Int, Int)] =
      curN match {
        case 0 | 1 => Some(curN, curN + 1)
        case _ => Some(getNextFib(curN - 1).getOrElse((0, 0))._1 + getNextFib(curN - 2).getOrElse((0,0))._1, curN + 1)
      }
    unfold(0)(getNextFib)
  }
}