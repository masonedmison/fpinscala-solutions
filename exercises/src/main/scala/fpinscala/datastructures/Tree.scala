package fpinscala.datastructures

sealed trait Tree[+A]


case class Leaf[A](value: A) extends Tree[A] {
  override def toString: String =
    value.toString
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def toString: String = this match {
    case Branch(l, r) =>
      s"\n/     \\\n${l.toString}     ${r.toString}"
  }
}

object Tree extends App {
  def size(t: Tree[Int]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    def maxAcc(tAcc: Tree[Int], curMax: Int): Int = {
      tAcc match {
        case Leaf(x) => curMax max x
        case Branch(l, r) => maxAcc(l, curMax) max maxAcc(r, curMax)
      }
    }

    maxAcc(t, -9999)
  }

  def depth[A](t: Tree[A]): Int = {
    def depthAcc(tAcc: Tree[A], pLen: Int): Int = {
      tAcc match {
        case Leaf(_) => pLen
        case Branch(l, r) => depthAcc(l, pLen + 1) max depthAcc(r, pLen + 1)
      }
    }

    depthAcc(t, 0)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold1[A, B](t: Tree[A], z: B)(op: (B, A) => B): B = {
    t match {
      case Leaf(x) => op(z, x)
      case Branch(l, r) => fold1(l, fold1(r, z)(op))(op)
    }
  }

  def fold2[A, B](t: Tree[A])(last: A => B)(op: (B, B) => B): B =
    t match {
      case Leaf(x) => last(x)
      case Branch(l, r) => op(fold2(l)(last)(op), fold2(r)(last)(op))
    }

  def sizeF2(t: Tree[Int]): Int = {
    fold2(t)(x => 1)(1 + _ + _)
  }

  def sizeF1(t: Tree[Int]): Int = {
    fold1(t, 0)((x, _) => x + 1)
  }

  def depthF2[A](t: Tree[A]): Int = {
    fold2(t)(x => 0)((d1, d2) => 1 + (d1 max d2))
    // mine below
/*
    t match {
      case Leaf(_) => 1
      case Branch(l, r) =>
        fold2(l)(x => 1)((x, _) => x + 1) max fold2(r)(x => 1)((x, _) => x + 1)
    }
*/
  }

  // TODO
  def depthF1[A](t: Tree[A]): Int =
    fold1(t, 0)((acc, _) => acc + 1)

  def maximumF1(t: Tree[Int]): Int = {
    fold1(t, -9999)((x, y) => x max y)
  }

  def maximumF2(t: Tree[Int]): Int = {
    fold2(t)(x => x)((x, y) => x max y)
  }

  def mapF2[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case _ =>
        fold2(t)(x => Leaf(f(x)): Tree[B])((x, y) => Branch(x, y))
    }
  }

  val t1 = Branch(Leaf(1), Leaf(2))
  val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  val t3 = Branch(Branch(Leaf(5), Leaf(4)), Branch(Leaf(7), Leaf(1)))
  val t4 = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))
  val t5 = Branch(Leaf(4), Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))

  println(s"size of t1 ${size(t1)}")
  println(s"size of t2 ${size(t2)}")

  println(s"max of t3 is ${maximum(t3)}") // 7
  println(s"max of t2 is ${maximum(t2)}") // 3

  println(s"depth of t3 is ${depth(t3)}") // 2
  println(s"depth of t4 is ${depth(t4)}") // 3

  val mappedT2 = map(t2)(_ + 1)
  println(s"map + 1 to t2 ${mappedT2}")
  println(s"map + 2 to t4 ${map(t4)(_ + 2)}")

  println(s"sizeF1 of t2 ${sizeF1(t2)}") // 2
  println(s"sizeF1 of t4 ${sizeF1(t4)}") // 4
  println(s"maxF1 of t3 is ${maximumF1(t3)}") // 7

  println(s"sizeF2 of t2 ${sizeF2(t2)}") // 2
  println(s"sizeF2 of t4 ${sizeF2(t4)}") // 4

  println(s"maxF2 of t3 is ${maximumF2(t3)}") // 7
  println(s"mapF2 + 1 to t4 ${map(t4)(_ + 1)}")

  println(s"depth2 of t3 is ${depthF2(t3)}") // 2
  println(s"depth2 of t4 is ${depthF2(t4)}") // 3
  println(s"depth2 of t5 is ${depthF2(t5)}") // 3
}