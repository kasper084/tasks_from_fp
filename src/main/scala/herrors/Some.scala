package herrors

case class Some[+A](get: A) extends Option[A]
