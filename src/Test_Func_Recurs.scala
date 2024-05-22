import scala.collection.mutable.ArrayBuffer

class cell(localisation: Position) {


}

class AdjacentCellFree(loc: Position, free: Boolean = true) extends cell(loc) {

  def adjacentCell(in: Array[Array[Int]], dir: Direction): Array[AdjacentCellFree] = {

    val result: Array[AdjacentCellFree] = Array.ofDim(3)
    var adjacentCell_possible: Array[Direction] = Array.ofDim(3)

    dir.orientation match {
      case 'N' => adjacentCell_possible = Array(new('O'), new('N'), new('E'))
      case 'S' => adjacentCell_possible = Array(new('O'), new('S'), new('E'))
      case 'O' => adjacentCell_possible = Array(new('O'), new('N'), new('S'))
      case 'E' => adjacentCell_possible = Array(new('S'), new('N'), new('E'))
    }

    for (pos: Int <- result.indices) {
      val mov: Position = adjacentCell_possible(pos).movement()

      val pos_X_CellAdjacent: Int = loc.x + mov.x
      val pos_Y_CellAdjacent: Int = loc.y + mov.y

      var cellAdjacentIsFree: Boolean = false

      if ((pos_X_CellAdjacent >= 0 && pos_X_CellAdjacent < in.length) && (pos_Y_CellAdjacent >= 0 && pos_Y_CellAdjacent < in.length)) {
        cellAdjacentIsFree = true
      }

      result(pos) = new AdjacentCellFree(new Position(pos_X_CellAdjacent, pos_Y_CellAdjacent), cellAdjacentIsFree)
    }

    return result
  }

  def oneOfAdjacentCell(in: Array[Array[Int]], dir: Direction) : AdjacentCellFree ={
    var result : AdjacentCellFree = null
    for (pos : Int <- in.indices) {
      val value : AdjacentCellFree = adjacentCell(in, dir)(pos)

      if(value.free){


      }

    }

  }


}


class Position(var x: Int, var y: Int) {

}

class Direction(var orientation: Char = 'N') {

  def randomDirInit(): Position = {
    var valueRandom: Int = (math.random() * 4).toInt
    if (valueRandom == 0) {
      valueRandom = 1
    }
    var result: Position = new Position(0, 0)

    valueRandom match {
      case 1 => result.y = 1
      case 2 => result.y = -1
      case 3 => result.x = -1
      case 4 => result.x = 1
    }

    return result
  }

  def movement(): Position = {
    var result: Position = new Position(0, 0)
    orientation match {
      case 'N' => result.y = 1
      case 'S' => result.y = -1
      case 'O' => result.x = -1
      case 'E' => result.x = 1
    }
    return result
  }


}


class Grid(in: Array[Array[Int]]) {

  def MaxNumberGrid(): Int = {

    var max: Int = 0

    for (lign: Int <- in.indices) {
      val value: Int = in(lign).max
      if (value >= max) {
        max = value
      }
    }
    return max
  }

  def fillInTheBoxes(in: Array[Array[Int]], orient: Direction, pos: Position): Array[Array[Int]] = {

    var posX: Int = pos.x + orient.movement().x
    var posY: Int = pos.y + orient.movement().y

    while (in(posX)(posY) == 0) {
      val maxNumAlreadyTakenInTheGrid: Int = MaxNumberGrid()
      in(posX)(posY) = maxNumAlreadyTakenInTheGrid + 1

      posX += orient.movement().x
      posY += orient.movement().y

    }
    return in
  }


}


class GridValid(var result: Array[Array[Int]], var valid: Boolean) extends Grid(result) {

}


object main extends App {

  val test: Array[Array[Int]] = Array.ofDim[Int](10, 10)

  def printGrid(in: Array[Array[Int]]): Unit = {
    for (lign: Int <- in.indices) {
      println(s"${in(lign).mkString(",")}")
    }
  }

  printGrid(test)

  def occupation(in: Array[Array[Int]]): Double = {
    var counter: Int = 0

    for (lign: Int <- in.indices) {
      for (column: Int <- in(lign).indices) {
        val value: Int = in(lign)(column)

        if (value == 0) {
          counter += 1
        }
      }
    }
    return 1 - counter / (in.length * in(0).length)
  }


  def generateGrid(in: Array[Array[Int]], dir: Direction): GridValid = {
    var finalGrid: Array[Array[Int]] = in

    val posDepart: Position = new Position((math.random() * in.length - 1).toInt, (math.random() * in.length - 1).toInt)


    val orientInitPos : Array[Direction] = Array(new Direction('N'),new Direction('S'),new Direction('E'),new Direction('W'))

    val directionMovementInit: Position = new AdjacentCellFree(posDepart).adjacentCell(in,orientInitPos((math.random()*3).toInt))()
    finalGrid(posDepart.x)(posDepart.y) = 1


    // Convergence de la fonction récursive
    if (occupation(finalGrid) >= 0.8) {
      return new GridValid(finalGrid, true)
    }

    else {
      new







      if (occupation(finalGrid) >= 0.8) {
        println("Résolu")
        return new GridValid(finalGrid, true)
      }


    }
    return
  }

  println("-----------------------------------------------------------------------------------------")
  printGrid(generateGrid(test))


}


