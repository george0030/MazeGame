package o1.adventure

import o1._
import o1.adventure.Maze.gridPosNeighbors
import o1.util.Random

import scala.io.StdIn.readLine


object MazeGame extends App{



  println("Enter the width of the maze. Minimum 7, recommended 15. Non-integer/too small will default to recommended value.")
  val width = readLine().toIntOption.getOrElse(15)
  println("Enter the width of the maze. Minimum 7, recommended 15. Non-integer/too small will default to recommended value.")
  val height = readLine().toIntOption.getOrElse(15)
  println("Enter maximum distance of the minotaur from the crown. 15 recommended.")
  val minotaurDistance = readLine().toIntOption.getOrElse(15)
  println("Enter viewdistance. 3 recommended.")
  val viewDistance  = readLine.toIntOption.getOrElse(3)

  val maze = Maze.generate(width,height, minotaurDistance, viewDistance)
  println("TYPE 'help' FOR INSTRUCTIONS!!")


  while(!maze.isOver){

    if(maze.godmode)
      maze.draw()
    else maze.printPlayerView()

    while(maze.playerHasTurn){
      maze.excecuteCommand(readLine().toLowerCase)


    }
      maze.minotaurTurn()
      maze.playerHasTurn = true



  }

  if(maze.hasWon) println("You live to tell the story of the maze for your children, I.E., you win. Thanks for playing")
  else println(" Game over. May the archaeologists of the 25th century find your bones.")

  System.exit(0)







}

object Maze{

  def generate(xSize: Int, ySize: Int, minoDistance: Int, viewDistance: Int): Maze = {

    def initializer(x: Int,y: Int) ={ if(x==0 || x == xSize-1 || y==0 || y == ySize-1) new EdgeWall() else new SoftWall()}




    var entranceX = 0
    var entranceY = 0
    val isEntranceX = Random.nextDouble() <= xSize.toDouble/(xSize+ySize).toDouble
    val upOrLeft = Random.nextBoolean()
    if(isEntranceX){
      entranceX = Random.nextInt(xSize-2)+1
      if(!upOrLeft) entranceY = ySize-1
    }
    else{
      entranceY = Random.nextInt(ySize-2)+1
      if(!upOrLeft) entranceX = ySize-1
    }


    val maze = new Maze(xSize, ySize, new GridPos(entranceX, entranceY), minoDistance, viewDistance, initializer)

    // Randomized Prim's algorithm
    val visited = Buffer[GridPos]()
    val walls = Buffer[GridPos]()
    var current = GridPos(entranceX, entranceY)
    var currentIndex = 0
    maze(current) = new Floor()
     visited += current
     walls.appendAll(gridPosNeighbors(current).filter(maze.contains(_)).filter(maze(_).isInstanceOf[SoftWall]))
    while(walls.nonEmpty){
      currentIndex = Random.nextInt(walls.size)
      current = walls(currentIndex)
      if(visited.count(gridPosNeighbors(_).contains(current)) <= 1){
      maze(current) = new Floor()
      visited += current
      walls.appendAll(gridPosNeighbors(current).filter(maze.contains(_)).filter(maze(_).isInstanceOf[SoftWall]))
      }
      walls.remove(currentIndex)
    }

    maze.crownPos = visited.last


    // Determines maximum distance of the spawning points of the minotaur and the crown
    def distanceComparison(x: GridPos) ={
      val distance = maze.path(x, visited.last).length
      distance <= minoDistance
    }
    maze.minotaurPos = visited.reverse.find(distanceComparison).get

    maze.playerPos = GridPos(entranceX, entranceY)

    maze


  }

  def gridPosNeighbors(pos:GridPos): Vector[GridPos] = {
    Vector(pos.neighbor(North), pos.neighbor(West), pos.neighbor(South), pos.neighbor(East))
  }

}

class Maze(xSize: Int, ySize: Int, val entrance: GridPos, minotaurDistance: Int, viewDistance: Int, initializer: (Int, Int) => Tile = (x,y)=>new Floor()) extends Grid[Tile](xSize,ySize){

  // This class is only instantiated through the companion object, meaning these are always defined. I understand why using null is bad :)
  var minotaurPos: GridPos = null
  var playerPos: GridPos = null
  var crownPos: GridPos = null
  var playerHasTurn = true
  var playerHasCrown = false
  var godmode = false
  var stunAvailable = true
  var minotaurStunned = 0


  override def initialElements: Seq[Tile] =for (y <- 0 until this.height; x <- 0 until this.width) yield initializer(x, y)

  def draw(): Unit ={
    val pathToVictory = if(playerHasCrown) path(playerPos, entrance) else path(playerPos, crownPos)
    var s = ""
    var i = 1
    for(e <- allPositions) {
      s += tileRepresentationGodMode(e, pathToVictory)
      if(i % xSize == 0)
        s += "\n"

    i += 1
    }
    println(s)
    println(if(playerHasCrown)"Objective: Escape!" else "Objective: Find the crown! ")

  }

  def printPlayerView(): Unit = {

    var s = ""
    var i = 1
    for(e <- allPositions) {
      val nextChar = if(playerPos.distance(e) <= viewDistance) tileRepresentation(e) else '~'
      s += nextChar
      if(i % xSize == 0)
        s += "\n"

    i += 1
    }
    println(s)
    println(if(playerHasCrown)"Objective: Escape!" else "Objective: Find the crown! ")

  }
  def tileRepresentation(tile: GridPos) = {

    if(tile == playerPos) 'H'
    else if(tile == minotaurPos) 'M'
    else if(tile == crownPos && !playerHasCrown) 'Å'
    else if(apply(tile).isInstanceOf[Floor]) '.'
    else apply(tile) match {
      case _: EdgeWall => 'O'
      case _: SoftWall => '#'
      case _ => '?'
    }





  }


  def tileRepresentationGodMode(tile: GridPos, pathToVictory: Vector[GridPos]) = {

    if(tile == playerPos) 'H'
    else if(tile == minotaurPos) 'M'
    else if(tile == crownPos && !playerHasCrown) 'Å'
    else if(apply(tile).isInstanceOf[Floor])
      if(pathToVictory contains tile) '$' else '.'
    else apply(tile) match {
      case _: EdgeWall => 'O'
      case _: SoftWall => '#'
      case _ => '?'
    }





  }

  def minotaurTurn(): Any = {
    if(minotaurStunned <= 0){
    if(minotaurPos == playerPos) minotaurPos = minotaurPos
    else if(playerHasCrown){
      minotaurPos = path(minotaurPos, playerPos).head
    }
    else{
        minotaurPos = Random.shuffle(gridPosNeighbors(minotaurPos).filter( x => contains(x) && apply(x).isInstanceOf[Floor]).filter(path(crownPos, _).length <= minotaurDistance)).head


    }
    }
    else minotaurStunned -= 1


  }

  def excecuteCommand(command: String): Unit = {

    command match{

      case "help" => println("Your Goal: Recover king Minos' crown from the maze. But beware the minotaur.")
      println("Once you take his crown, it will start chasing you until you manage to escape or you end up as its dinner. More specifically, you lose if you share a tile with the minotaur while it is not stunned.")
      println("You do, however have a magnesium flare which can distract the minotaur for 2 turns.")
      println("You win if you manage to escape the maze with the crown")
      println("Remember! Patience is key! You'll need to look for the right opportunity to strike!")
      println("Map legend:")
      println("~ -> Unknown territory")
      println("O and #  ->  Walls")
      println(". -> Floor")
      println("H -> You")
      println("M  -> Minotaur")
      println("Å  -> Crown")
      println("Commands:")
      println("help -> Shows the objective, map legend and commands")
      println("map -> Reprints the map")
      println("take -> Takes an item (the crown)")
      println("stun -> Stuns the minotaur (you must be next to it)! One-time use only.")
      println("'skip' or 'z' -> Skip a turn")
      println("w -> Move north")
      println("a -> Move west")
      println("s -> Move south")
      println("d -> Move east")

      case "map" => if(godmode) draw() else printPlayerView()
      case "take" => if (playerPos == crownPos && !playerHasCrown){
        playerHasCrown = true
        println("It's a bit dark to see clearly, but the distinct glow along with its shape and heavy weight mean you must have found what you are looking for.")
        println("You hear a monstrous cry nearby!")
      }
      case "stun" => if(playerPos.distance(minotaurPos) <= 1){
        if(stunAvailable){
        println("You light up your flare, almost blinding you, but giving you a few crucial seconds to run for your life")
        minotaurStunned = 2
        stunAvailable = false}
        else println("You already used it. Too bad.")
      }
      else println("You must be next to the minotaur to stun it!")
      case "skip" => playerHasTurn = false
      case "z" => playerHasTurn = false


      case "w" => if(canMove(playerPos, North)){
        println("You move north")
        playerPos = playerPos.neighbor(North)
        playerHasTurn = false
      }
      else println("You can't move there")

      case "a" => if(canMove(playerPos, West)){
        println("You move west")
        playerPos = playerPos.neighbor(West)
        playerHasTurn = false
      }
      else println("You can't move there")

      case "s" => if(canMove(playerPos, South)){
        println("You move south")
        playerPos = playerPos.neighbor(South)
        playerHasTurn = false
      }
      else println("You can't move there")
      case "d" => if(canMove(playerPos, East)){
        println("You move east")
        playerPos = playerPos.neighbor(East)
        playerHasTurn = false
      }
      else println("You can't move there")
      case "godmode" =>{
        println("If you REALLY must.")
        godmode = true}
      case "sorry" =>{
        println("Blessed are the meek, for will they inherit the earth.")
        godmode = false}
      case _ => println("Invalid command")


    }

  }
  def hasWon = playerHasCrown && playerPos == entrance
  def hasLost = if(playerPos == minotaurPos && minotaurStunned<=0 && !godmode) true else false
  def isOver = hasWon || hasLost

// Finds the (shortest?) path from a to b. Uses the wall following algorithm while removing dead ends.
  def path(a: GridPos, b:GridPos): Vector[GridPos] ={
    var heading = North
    var latest = a
    var track = Buffer[GridPos](a)
    var latestPreviousIndex = 0
    while(!(latest == b)){

      if(canMove(latest, toTheRightOf(heading))){
        heading = toTheRightOf(heading)
      }
      else if(canMove(latest, heading)){
      }
      else if(canMove(latest, toTheLeftOf(heading))){
        heading = toTheLeftOf(heading)
      }
      else{
        heading = toTheLeftOf(toTheLeftOf(heading))
      }
      latest = latest.neighbor(heading)
      latestPreviousIndex = track.indexOf(latest)
      if(latestPreviousIndex >= 0){
       track = track.take(latestPreviousIndex)
      }
      track.append(latest)

    }

  track.tail.toVector

  }

  def canMove(location: GridPos, target: CompassDir) = contains(location.neighbor(target)) && apply(location.neighbor(target)).isInstanceOf[Floor]

    private def toTheRightOf(direction: CompassDir) ={
      if(direction == North) East
      else if (direction == East) South
      else if (direction == South) West
      else North


    }
    private def toTheLeftOf(direction: CompassDir) ={
      if(direction == North) West
      else if (direction == West) South
      else if (direction == South) East
      else North

    }


}



