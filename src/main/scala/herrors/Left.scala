package herrors

case class Left[+E](value: E) extends Either[E, Nothing]
