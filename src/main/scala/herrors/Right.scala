package herrors

case class Right[+A](value: A) extends Either[Nothing, A]
