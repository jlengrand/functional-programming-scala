object myModule {
  def abs(n: Int): Int =
    if (n < 0) -n else n

  def factorial(n: Int): Int = {

  def fact(n: Int, acc: Int) : Int = {
    if(n == 1) acc
    else fact(n - 1, acc * n)
  }

    fact(n, 1)
  }

  def fibonacci(n: Int) : Int = {
    def go(n:Int, prev:Int, curr: Int): Int = {
      if(n == 0) prev
      else
        go(n-1, curr, prev + curr )
    }

    go(n, 0, 1)
  }

  private def formatAbs(x: Int) ={
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit ={
    println(formatAbs(-42))
  }
}
