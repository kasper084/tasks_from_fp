package fibonacci

import scala.annotation.tailrec
import scala.io.StdIn.readInt

object FibonacciNum extends App {

  val res = fib(inputNum())
  println(res)

  def inputNum(): Long = {
    println("Enter some int")
    val num = readInt();
    num
  }

  //exercise 1
  def fib(fibNum: Long): Long = {
    @tailrec
    def someHelp(num: Long, numA: Long, numB: Long): Long = {
      if (num <= 0) numA + numB
      else someHelp(num - 1, numB, numA + numB)
    }

    if (fibNum < 2) 1 else someHelp(fibNum - 2, 1, 1)
  }
}
