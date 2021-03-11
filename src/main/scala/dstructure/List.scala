package dstructure

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  //exercise 1 what will be the result of matching?
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x //I was thinking about this variant:
    // oh, "x" then cons2 - at first glance it looks right
    case Nil => 42 // "x" is list of Int-s not Nothing and Int extends AnyVal
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)  // what? copy of def sum case?
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
}
