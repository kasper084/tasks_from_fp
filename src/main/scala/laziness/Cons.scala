package laziness

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
