package dstructure

import dstructure.DataStructure.List

case class Cons[+A](head: A, tail: List[A]) extends List[A]
