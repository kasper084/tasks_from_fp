package laziness

import scala.collection.immutable.Stream.cons

trait Stream[+A] {

  case object Empty extends Stream[Nothing]

  //exercise 1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  //exercise 2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  //exercise 3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      val hh = h()
      if (p(hh)) Stream.cons(hh, t().takeWhile(p))
      else Empty
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  //exercise 4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  //exercise 5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) =>
      if (p(h)) cons(h, t)
      else Empty
    )

  //exercise 6
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) =>
      if (p(h)) cons(h, t)
      else t
    )

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => f(h).append(t))

  object Stream {

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = cons(1, ones)

    //exercise 7
    def constant[A](a: A): Stream[A] =
      cons(a, constant(a))

    //exercise 8
    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))

    //exercise 9
    def fibs: Stream[Int] = {
      def innerFibs(a: Int, b: Int): Stream[Int] = {
        cons(a, innerFibs(b, a + b))
      }

      innerFibs(0, 1)
    }
  }

  //exercise 10
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Empty
  }

  //exercise 11
  def fibs2: Stream[Int] =
    unfold((0, 1))(x => Some((x._1, (x._2, x._1 + x._2))))

  def from2(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  def constant2[A](a: A): Stream[A] =
    unfold(a)(x => Some(x, x))


  //exercise 12
  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def take2(n: Int): Stream[A] =
    unfold((this, n))(x => x._1 match {
      case Cons(h, t) if x._2 > 0 => Some((h(), (t(), x._2 - 1)))
      case _ => None
    })

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None: Option[B]), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None: Option[A], Some(h2())), (Empty, t2()))
      case _ => None
    }

  //exercise 13
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2 != None).forAll {
      case (Some(x), Some(y)) if x == y => true
      case _ => false
    }

  //exercise 14
  def tails: Stream[Stream[A]] =
    unfold(this)(x => x match {
      case Cons(h, t) => Some(x, t())
      case Empty => None
    }) append (Stream(Empty))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  //exercise 15
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
      val b2 = f(a, b._1)
      (b2, cons(b2, b._2))
    })._2

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

}
