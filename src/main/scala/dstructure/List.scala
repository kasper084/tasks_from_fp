package dstructure

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  //exercise 1
  // what will be the result of matching?
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x //I was thinking about this variant:
    // oh, "x" then cons2 - at first glance it looks right
    case Nil => 42 // "x" is list of Int-s not Nothing and Int extends AnyVal
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t) // what? copy of def sum case?
    // why and how it will be matching
    case _ => 101 // list is consisted Int-s matching should go fine, so not this case
    //it`s not Nil, not _ and not Cons(h,t) and I don't like how first case looks
    //going this way left us with only 3 variant left
    //Cons takes list, head is first element and tail is everything after
    //Cons(x, =  1
    //and then we call Cons again
    //Cons(y, = 2
    //x + y = 3
  }

  //exercise 2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("invalid") // whatever
      //case Nil => Nil
      case Cons(_, t) => t
    }

  //exercise 3
  def drop[A](n: Int, l: List[A]): List[A] = {
    if (n <= 0) l
    else drop(n - 1, tail(l))
  }

  //exercise 4
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else l
  }

  //exercise 5
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("invalid") // whatever
    //case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  //exercise 6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  //exercise 7
  // I don`t know it's a mess, looks like we would have error on a large list,
  // function would calls itself too many times, stack will be full
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


  //exercise 10
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  //exercise 12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
}

