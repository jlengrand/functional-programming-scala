package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](leftTree: Tree[A], rightTree: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => (depth(l) max depth(r)) + 1
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf (v) => f(v)
      case Branch (l, r) => g(fold(l)(g), fold(r)(g))
    }
  }

  def size2[A](t: Tree[A]): Int = {
    fold(t)(a => 1)(1 + _ + _ )
  }

  def maximum2(t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _ )
  }

  def depth[A](t: Tree[A]): Int = {
    fold(t)(a => 1)((l, r) => 1 + (l) max (r))
  }
}