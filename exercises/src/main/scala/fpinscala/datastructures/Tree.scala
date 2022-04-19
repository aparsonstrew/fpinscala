package fpinscala.datastructures

import fpinscala.datastructures.Tree.{depth, depth2, maximum, maximum2, size, size2}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold[A, B](l)(f)(g), fold[A, B](r)(f)(g))
    }
  }

  def size2[A](t: Tree[A]): Int =
    fold[A, Int](t)(_ => 1)(_ + _ + 1)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(a => a)(_.max(_))

  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 0)(_.max(_) + 1)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(a => Leaf(f(a)): Tree[B])((b1, b2) => Branch(b1, b2))
  }


}

object Testing extends App {
  val tree = Branch(Branch(Leaf(12), Leaf(-5)), Branch(Leaf(0), Branch(Leaf(2), Leaf(-34))))

  val a1 = size(tree)
  val a2 = size2(tree)
  println(a1)
  println(a2)

  val b1 = maximum(tree)
  val b2 = maximum2(tree)
  println(b1)
  println(b2)

  val c1 = depth(tree)
  val c2 = depth2(tree)
  println(c1)
  println(c2)


}