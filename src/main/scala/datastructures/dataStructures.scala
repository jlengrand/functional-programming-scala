package datastructures

object dataStructures {

  def main(args: Array[String]): Unit = {

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    println(x)

    println(List.tail(List(1, 2, 3, 4, 5)))
    println(List.tail(Nil))

    println(List.setHead(Nil, 3))
    println(List.setHead(List(1, 2), 3))

    println(List.drop(List(1, 2, 3, 4, 5), 3))
    println(List.drop(Nil, 3))

    println(List.dropWhile(List(1, 2, 3, 4, 2))( x => (x < 2)))

    println(List.init(List(1, 2, 3, 4, 5)))
    println(List.length(List(1, 2, 3, 4, 5)))

    println(List.sum2(List(1, 2, 3, 4, 5)))
    println(List.product2(List(1, 2, 3, 4, 5)))
    println(List.sum3(List(1, 2, 3, 4, 5)))
    println(List.product3(List(1, 2, 3, 4, 5)))


    println(List.append2(List(1, 2, 3, 4, 5), List(6, 7, 8)))

    println(List.addOne(List(1, 2, 3, 4, 5)))
    println(List.doubleToString(List(1.3, 2, 3, 4, 5)))

    println(List.filter(List(1, 2, 3, 4, 5, 2, 3))(x=> x<3))
  }
}
