import scala.collection.mutable.ArrayBuffer


//class Cell(localisation: Position) {}

//class AdjacentCellFree(loc: Position, f: Boolean = true) extends Cell(loc) {

  //val free: Boolean = f
//val localisation: Position = loc

//}


//class Position(var x: Int, var y: Int) {}

//class Direction(or: Char) {

  //val orientation = or
/*
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
 // }


}


class ToolsGrid(in: Array[Array[Int]]) {
/*
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

  */

  def fillInTheBoxes(orient: Direction, pos: Position): Array[Array[Int]] = {

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

  def adjacentCell(in: Array[Array[Int]], dir: Direction, loc: Position): Array[AdjacentCellFree] = {

    val result: Array[AdjacentCellFree] = Array.ofDim(3)
    var adjacentCell_possible: Array[Direction] = Array.ofDim(3)

    dir.orientation match {
      case 'N' => {
        adjacentCell_possible = Array(new Direction('O'), new Direction('N'), new Direction('E'))
      }
      case 'S' => {
        adjacentCell_possible = Array(new Direction('O'), new Direction('S'), new Direction('E'))
      }
      case 'O' => {
        adjacentCell_possible = Array(new Direction('O'), new Direction('N'), new Direction('S'))
      }
      case 'E' => {
        adjacentCell_possible = Array(new Direction('S'), new Direction('N'), new Direction('E'))
      }
    }

    for (pos: Int <- result.indices) {
      val mov: Position = adjacentCell_possible(pos).movement()

      val pos_X_CellAdjacent: Int = loc.x + mov.x
      val pos_Y_CellAdjacent: Int = loc.y + mov.y

      var cellAdjacentIsFree: Boolean = false

      if ((pos_X_CellAdjacent >= 0 && pos_X_CellAdjacent < in.length) && (pos_Y_CellAdjacent >= 0 && pos_Y_CellAdjacent < in.length) && in(pos_X_CellAdjacent)(pos_Y_CellAdjacent) == 0) {
        cellAdjacentIsFree = true
      }

      result(pos) = new AdjacentCellFree(new Position(pos_X_CellAdjacent, pos_Y_CellAdjacent), cellAdjacentIsFree)
    }

    return result
  }

  def oneOfAdjacentCell(in: Array[Array[Int]], dir: Direction, loc: Position): Position = {
    var result: AdjacentCellFree = null
    for (pos: Int <- in.indices) {
      val value: AdjacentCellFree = adjacentCell(in, dir,loc)(pos)

      if (value.free) {
        result = value
      }

    }

    return result.localisation
  }


}


class GridValid(var result: Array[Array[Int]], var valid: Boolean) extends ToolsGrid(result) {

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
      var toolsGrid = new ToolsGrid(finalGrid)


      val posDepart: Position = new Position((math.random() * in.length - 1).toInt, (math.random() * in.length - 1).toInt)


      val orientInitPossible : Array[Direction] = Array(new Direction('N'),new Direction('S'),new Direction('E'),new Direction('W'))
      val orientInit : Direction = orientInitPossible((math.random()*3).toInt)
      val directionMovementInit: Position = toolsGrid.oneOfAdjacentCell(in,orientInit)


      finalGrid(posDepart.x)(posDepart.y) = 1
      finalGrid = new ToolsGrid(finalGrid).fillInTheBoxes(orientInit,directionMovementInit)


      // Convergence de la fonction récursive
      if (occupation(finalGrid) >= 0.8 ) {
        return new GridValid(finalGrid, true)
      }

      else {}
      return
    }

  println("-----------------------------------------------------------------------------------------")
  printGrid(generateGrid(test))


}
*/

