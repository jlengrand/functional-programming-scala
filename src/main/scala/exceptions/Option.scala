package exceptions

sealed trait Option[+A] {

  def map[B](f : A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  def flatMap[B](f : A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default : => B): B =
    this match {
      case None => default
      case Some(a) => a
    }


  def orElse[B >: A](ob : => Option[B]): Option[B] = {
    this match {
      case None => ob
      case _ => this
    }
  }

  def filter(f: A => Boolean) : Option[A] = {
    this match {
      case Some(a) if (f(a)) => this
      case None => None
    }
  }

  def variance(s : Seq[Double]) : Option[Double] = {
    def mean(l: Seq[Double]) : Option[Double] = {
      if (s.isEmpty) None
      else Some(l.sum / l.length)
    }

    mean(s) flatMap (m => mean(s.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] = {
    a flatMap (v => (b map (w => f(v, w) )))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

  object Option {
    def main(args: Array[String]): Unit = {

      println(Some(2) map (x => x * 2))
      println(Some(2) getOrElse (6))
      println(None getOrElse (6))
    }
  }