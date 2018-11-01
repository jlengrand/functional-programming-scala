package strictnessandlaziness

sealed trait Stream[+A]{
  import Stream._

  def toList: List[A] = {
    this match {
      case Empty => List()
      case Cons(h, t) => h() :: t().toList
    }
  }

  def take(n: Int) : Stream[A] = {
    this match {
      case Empty => empty
      case Cons(h, t) => if (n == 1) cons(h(), empty) else cons(h(), t().take(n - 1))
    }
  }

  def drop(n: Int) : Stream[A] = {
    this match {
      case Empty => this
      case Cons(h, t) => if (n > 0) t().drop(n - 1) else this
    }
  }

  def takeWhile(f: A => Boolean): Stream[A] = {
    this match {
      case Empty => empty
      case Cons(h, t) => if (f(h())) cons(h(), empty) else cons(h(), t().takeWhile(f))
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match{
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def exists(p : A => Boolean) : Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def forAll(p : A => Boolean) : Boolean = {

  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=> A, t: ()=> Stream[A]) extends Stream[A]

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl : => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(()=> head, () => tail)
  }

  def fromList[A](l : List[A]): Stream[A] = {
    if(l.isEmpty) empty
    else{
      cons(l.head, fromList(l.drop(1)))
    }

  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    val plopList = List(1, 2, 3, 4)
    val plop = cons(1, cons(2, cons(3, empty)))

    println(plop)
    println(Stream.fromList(plopList))
    println(plop.toList)
    println(plop.take(1).toList)
    println(plop.drop(1).toList)
    println(plop.takeWhile(x => x < 2).toList)
    println(plop.exists(x => x > 3))
    println(plop.exists(x => x < 4))
  }
}
