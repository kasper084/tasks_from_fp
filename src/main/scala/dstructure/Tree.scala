package dstructure

trait Tree[+A]

object Tree {

  //exercise 25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // Exercise 26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  //exercise 27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => depth(left).max(depth(right)) + 1
  }

  //exercise 28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //exercise 29
  def fold[A, B](t: Tree[A])(f: A => B)(b: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => b(fold(l)(f)(b), fold(r)(f)(b))
  }

}