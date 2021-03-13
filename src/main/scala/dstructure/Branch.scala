package dstructure

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
