import fpinscala.datastructures.{Cons, Nil, List}

import scala.annotation.tailrec

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

@tailrec
def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
  l match {
    case Nil => z
    case Cons(x, xs) =>
      foldLeft(xs, f(z, x))(f)
  }
}

def lengthR[A](l: List[A]): Int = {
  foldRight(l, 0)((_, acc) => acc + 1)
}

def lengthL[A](l: List[A]): Int = {
  foldLeft(l, 0)((acc, _) => acc + 1)
}

// 3.12
def revList[A](l: List[A]): List[A] =
  foldLeft(l, Nil:List[A])((l: List[A], y: A) => Cons(y, l))


def swapF[A, B](f: (A, B) => B): (B, A) => B = (y:B, x:A) => f(x, y)

// 3.13 - foldRight in terms of foldLeft
def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
  l match {
  case Nil => z
  case _ =>
    foldLeft(revList(l), z)((b: B, a: A) => f(a, b))
  }
}

// 3.15 - append in terms of foldLeft or foldRight
def appendR[A, B](l: List[A], el: A): List[A] =
  foldRight(l, List(el))(Cons(_, _))

// 3.15 - append in terms of foldLeft or foldRight
def appendL[A](l: List[A], el: A): List[A] =
  foldLeft(revList(l), List(el))((x, y) => Cons(y, x))

// 3.15 - concatenate lists
def concat[A](ls: List[List[A]]): List[A] = {
  ls match {
    case Nil => Nil
    case Cons(xs: List[A], ys: List[List[A]]) =>
      foldRight(xs, concat(ys))(Cons(_, _))
  }
}

// 3.16 add one to each element
def addOne(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case Cons(x, xs) => Cons(x + 1, addOne(xs))
}

// 3.17
def doubToString(l: List[Double]): List[String] = l match {
  case Nil => Nil
  case Cons(x, xs) => Cons(x.toString, doubToString(xs))
}

// 3.18
def map[A, B](as: List[A])(f: A => B): List[B] = as match {
  case Nil => Nil
  case Cons(x, xs) => Cons(f(x), map(xs)(f))
}

// 3.19 filter
def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
  case Nil => Nil
  case Cons(x, xs) =>
    if (f(x)) Cons(x, filter(xs)(f))
    else filter(xs)(f)
}

// 3.20 flatmap
/*
def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
  case Nil => List()
  case Cons(x: A, xs: List[A]) =>
}
*/

// 3.22
def addByElem(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
  case (Nil, Nil) => Nil
  case (Cons(x, xs), Cons(y, ys)) =>
    Cons(x + y, addByElem(xs, ys))
}

// 3.23
def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (Cons(x, xs), Cons(y, ys)) =>
    Cons(f(x, y), zipWith(xs, ys)(f))
}

// 3.24
// assumes l1 and l2 are sorted and elements are distinct (no dups)
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  @tailrec
  def hasSubAcc(supAcc: List[A], subAcc:List[A]): Boolean = (supAcc, subAcc) match {
    case (_, Nil) => true
    case(Nil, _) => false
    case(Cons(x, xs), Cons(y, ys)) =>
      if(x == y) hasSubAcc(xs, ys)
      else hasSubAcc(xs, subAcc)
  }
  hasSubAcc(sup, sub)
}



// testing the above funcs
foldRight2(List(1, 2, 3), Nil:List[Int])(Cons(_, _))
foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))

foldLeft(List(1,2,3, 4), 0)(_ + _)

lengthL(List(1,2,3, 4))
lengthL(List(1,2,3,4,5))

revList(List(1,2,3))

appendR(List(1,2,3), 4)
appendL(List(1,2,3), 4)

val ll1 = List(List(1,2,3), List(4,5,6))
concat(ll1)
addOne(List(1,2,3))

// filter odd numbers
filter(List(1,2,3,4,5))(_ % 2 == 0)
addByElem(List(1,2,3), List(4,5,6))

zipWith(List(1,2,3), List(4,5,6))(_ + _)

hasSubsequence(List(1,2,3,4), List(2,3)) // true

hasSubsequence(List(1,2,3,4), List(2,3,4)) // true

hasSubsequence(List(1,2,3,4), List(5)) // false