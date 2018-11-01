package datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a2, a1)((l, v) => Cons(l, v))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match{
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Cons(h, Nil)
      case _ => Cons(h, l)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }


  def length[A](l: List[A]): Int = foldRight(l, 0)((_, x)=> x + 1)

  def reverse[A](l: List[A]) : List[A] = {
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc) )
  }

  def flatten[A](l: List[List[A]]) : List[A] = {
    foldLeft(l, Nil: List[A])(append)
  }

  def addOne(l : List[Int]) : List[Int] ={
    foldRight(l, List[Int]())((h, acc) => Cons(h+1, acc))
  }

  def doubleToString(l : List[Double]) : List[String] ={
    foldRight(l, List[String]())((h, acc) => Cons(h.toString, acc))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((h, acc) => Cons(f(h), acc))
  }

  def filter[A](l: List[A])(f: A => Boolean) : List[A] ={
    foldRight(l, List[A]())(
      (h, acc) => if(f(h)) Cons(h, acc) else acc
    )
  }

  def flatMap[A, B](l : List[A])(f : A => List[B]) : List[B] = {
    flatten(map(l)(f))
  }

  def filterFlatMAp[A](l: List[A])(f: A => Boolean) : List[A] = {
    flatMap(l)(a => if(f(a)) List(a) else Nil)
  }

  def addPerElement(l: List[Int], l2 : List[Int]) : List[Int] ={
    (l, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPerElement(t1,t2))
    }
  }

  def zipWith[A, B, C](l: List[A], l2 : List[B])(f: (A, B) => C): List[C] ={
    (l, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }
  }
}
