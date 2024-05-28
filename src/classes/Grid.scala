package classes

import scala.collection.mutable.ArrayBuffer

object test extends App {
  var grid: Grid = new Grid(4, 6)
  grid.display()
  val result: Array[Position] = grid.adjacentCell()
  for (quoi <- result) {
    println(s"${quoi.x}, ${quoi.y}")

  }


}

class Grid(x: Int, y: Int) {
  var grid: ArrayBuffer[ArrayBuffer[Cellule]] = ArrayBuffer()


  for (idy <- 0 until y) {
    grid.addOne(new ArrayBuffer[Cellule]())
    for (idx <- 0 until x) {
      grid(idy).addOne(new Cellule())
    }
  }
  grid(1)(0).setValueInt(1)


  def display(): Unit = {
    for (y <- grid.indices) {
      for (x <- grid(y).indices) {
        if (!grid(y)(x).isObstacl) {
          if (grid(y)(x).haveSquatter) {
            print(grid(y)(x).getValueInt + " ")
          } else {
            print("0 ")
          }
        }
        else {
          print("X ")
        }
      }
      println()
    }
  }

  def gridIsFinish: Boolean = {
    for (y <- grid.indices) {
      for (x <- grid(y).indices if (grid(y)(x).getValueInt == 0)) {
        return false
      }
    }
    true
  }

  def move(action: Direction): Unit = {
    var headX: Int = getHeadPos(1)
    var headY: Int = getHeadPos(0)

    if (action.actionX + headX >= grid(0).length || action.actionX + headX < 0 || action.actionY + headY >= grid.length || action.actionY + headY < 0) {
      return
    }

    if (grid(headY + action.actionY)(headX + action.actionX).isObstacl || grid(headY + action.actionY)(headX + action.actionX).haveSquatter)
      return


    grid(headY + action.actionY)(headX + action.actionX).setValueInt(grid(headY)(headX).getValueInt + 1)
    move(action)
  }

  def getHeadPos: Array[Int] = {
    var res: Array[Int] = Array(0, 0)
    var maxVal: Int = 0;
    for (y <- grid.indices) {
      for (x <- grid(y).indices if (grid(y)(x).getValueInt > maxVal)) {

        maxVal = grid(y)(x).getValueInt
        res = Array(y, x)
      }
    }
    res
  }

  // Fonction pour avoir les localisations des différentes cellules adjacentes possibles, pour ce déplacer
  def adjacentCell(): Array[Position] = {
    var headX: Int = getHeadPos(1)
    var headY: Int = getHeadPos(0)

    val listAdjacentCell: ArrayBuffer[Position] = new ArrayBuffer[Position]()

    for (i: Int <- -1 to 1; if (i != 0)) {

      val headYC: Int = stayInTheGrid(headY + i)
      val headXC: Int = stayInTheGrid(headX + i)

      val posUD: Position = new Position(headX, headYC)
      val posRL: Position = new Position(headXC, headY)

      val moveRL: Cellule = grid(posRL.y)(posRL.x)
      val moveUD: Cellule = grid(posUD.y)(posUD.x)

      if (!moveRL.haveSquatter && !moveRL.isObstacl && headX + i == headXC) {
        listAdjacentCell.addOne(posRL)
      }
      if (!moveUD.haveSquatter && !moveUD.isObstacl && headY + i == headYC) {
        listAdjacentCell.addOne(posUD)
      }

    }

    return listAdjacentCell.toArray
  }

  // Fonction qui permet de remplacer un chiffre négatif, par 0. Utilisé dans la fonction adjacentCell(), pour ne pas avoir de coordonnée négative
  def stayInTheGrid(nb: Int): Int = {
    if (nb >= 0) {
      return nb
    }
    else return 0
  }

}