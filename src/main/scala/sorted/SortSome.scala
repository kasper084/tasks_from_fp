package sorted

import scala.annotation.tailrec

object SortSome extends App {

  //exercise 2
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def goLooping(num: Int): Boolean =
      if (num >= as.length - 1) true
      else if (gt(as(num), as(num))) goLooping(num + 1)
      else false

    goLooping(10)
  }

  //exercise 3
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b:B) => f(a,b)

  //exercise 4
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  //exercise 5 - should be something like exercise 4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  //exercise 6
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
