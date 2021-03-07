package player

object Winner extends App {

  val players = List(Player("One", 5),
    Player("Two", 4),
    Player("Three", 3))

  runTasks()

  def runTasks(): Unit = {
    fromListOfPlayers(players)
  }

  def fromListOfPlayers(list: List[Player]): Unit = {
    println(list.reduceLeft(winner))
  }


  def winner(p1: Player, p2: Player): Player = {
    if (p1.score > p2.score) p1 else p2
  }

  def printWinner(p: Player) = println(s"${
    p.name
  } is winner!")

  def declareWinner(p1: Player, p2: Player): Unit = printWinner(winner(p1, p2))
}

