package dstructure

import DataStructure.List.x

import scala.annotation.tailrec

object DataStructure extends App {

  trait List[+A]

  case object Nil extends List[Nothing]

  object List {

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    //exercise 1
    //what will be the result of matching?
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x //I was thinking about this variant:
      //oh, "x" then cons2 - at first glance it looks right
      case Nil => 42 // "x" is list of Int-s not Nothing and Int extends AnyVal
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t) //copy of def sum case?
      //why and how it will be matching
      case _ => 101 // list is consisted Int-s matching should go fine, so not this case
      //it`s not Nil, not _ and not Cons(h,t) and I don't like how first case looks
      //going this way left us with only 3 variant left
      //Cons takes list, head is first element and tail is everything after
      //Cons(x, =  1
      //and then we call Cons again
      //Cons(y, = 2
      //x + y = 3
    }
  }

  println(x)

  //exercise 2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    //case Nil => sys.error("invalid")
    //whatever
    case Cons(_, t) => t
  }

  //exercise 3
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("invalid")
    case Cons(_, t) => Cons(h, t)
  }

  //exercise 4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  //exercise 5
  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  //exercise 6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("invalid") //whatever
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  //exercise 7
  //hm, looks like we would have error on a large list
  //similar to the example in Mark Lewis fibonacci video
  //function would calls itself too many times, stack will be full
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  //exercise 8
  //Hm, what I thing about bla bala this and bla bla that...
  //I will forget about this task few hours late, it's 00:15 already
  //give me a break

  //exercise 9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  //exercise 10
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  //exercise 11
  def sumWithFoldLeft[A](l: List[Int]): Int =

    foldLeft(l, 0)(_ + _)

  def prdctWithFoldLeft[A](l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lenWithFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  //exercise 12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  //exercise 13
  def foldRightViaLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldLeftViaRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((x, y) => f(y, x))

  //exercise 14
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  //exercise  16
  def inc(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  //exercise 17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  //exercise 18
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  //exercise 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => {
      if (f(h)) Cons(h, t)
      else t
    })

  //exercise 20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((h, t) => append(f(h), t))

  //exercise 21
  //yep
  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x =>
      if (f(x)) List(x)
      else Nil
    )

  //exercise 22
  def sumEach(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sumEach(xs, ys))
  }

  //exercise 23
  def zip[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zip(t1,t2)(f))
  }
  //exercise 24
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
}