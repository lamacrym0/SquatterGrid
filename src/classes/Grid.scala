package classes

import jdk.jfr.Percentage

import scala.collection.mutable.ArrayBuffer

object test extends App {
  val columnGrid: Int = 5
  val lignGrid: Int = 6
  var gridTest: Grid = new Grid(columnGrid, lignGrid)


  gridTest.display()

  println("----------------------------------------------------------------------")

  var percentMini: Double = 0.8


  gridTest.generateGrid(percentMini, ((0.13 * (columnGrid * lignGrid))).toInt)
  gridTest.display()


}

class Grid(x: Int, y: Int) {
  var grid: ArrayBuffer[ArrayBuffer[Cellule]] = ArrayBuffer()


  for (idy <- 0 until y) {
    grid.addOne(new ArrayBuffer[Cellule]())
    for (idx <- 0 until x) {
      grid(idy).addOne(new Cellule())
    }
  }
  grid(0)(0).setValueInt(1)


  def resetGrid():Unit ={
    for(y<-grid.indices;x<-grid(y).indices if(grid(y)(x).getValueInt != 0 || grid(y)(x).getValueInt != 1)){
      grid(y)(x).setValueInt(0)
    }
  }

  def display(): Unit = {
    print("\r")
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
      print("\n")
    }
  }

  def headCanMove(): Boolean = {
    var headX: Int = getHeadPos.x
    var headY: Int = getHeadPos.y

    var actions: Array[Direction] = Array(North(), South(), East(), West())

    for (action <- actions) {
      if (!(action.actionX + headX >= grid(0).length || action.actionX + headX < 0 || action.actionY + headY >= grid.length || action.actionY + headY < 0)) {
        if (!(grid(headY + action.actionY)(headX + action.actionX).isObstacl || grid(headY + action.actionY)(headX + action.actionX).haveSquatter)) {
          return true

        }
      }
    }
    false
  }

  def gridIsFinish: Boolean = {
    for (y <- grid.indices) {
      for (x <- grid(y).indices if (grid(y)(x).getValueInt == 0)) {
        return false
      }
    }
    true
  }

  def move(action: Direction): Int = {
    var headX: Int = getHeadPos.x
    var headY: Int = getHeadPos.y

    if (action.actionX + headX >= grid(0).length || action.actionX + headX < 0 || action.actionY + headY >= grid.length || action.actionY + headY < 0) {
      return 0
    }

    else if (grid(headY + action.actionY)(headX + action.actionX).isObstacl || grid(headY + action.actionY)(headX + action.actionX).haveSquatter) {
      return 0
    }
    else {
      grid(headY + action.actionY)(headX + action.actionX).setValueInt(grid(headY)(headX).getValueInt + 1)
      return 1 + move(action)
    }
  }

  def getHeadPos: Position = {
    var res: Position = new Position(0, 0)
    var maxVal: Int = 0;
    for (y <- grid.indices) {
      for (x <- grid(y).indices if (grid(y)(x).getValueInt > maxVal)) {

        maxVal = grid(y)(x).getValueInt
        res = new Position(x, y)
      }
    }
    res
  }

  // Fonction pour calculer le pourcentage d'occupation de la surface par le "Squatter"
  def occupation(): Double = {
    var counter: Double = 0
    var counterObstacle: Double = 0
    val surfaceInit: Double = (grid.length) * (grid(0).length)

    for (lign: Int <- grid.indices) {
      for (column: Int <- grid(lign).indices) {
        val value: Cellule = grid(lign)(column)


        if (value.getValueInt != 0 && !value.isObstacl) {
          counter += 1.0
        }

        else if (value.isObstacl) {
          counterObstacle += 1
        }

      }
    }

    return ((counter) / (surfaceInit - counterObstacle))
  }


  // Fonction pour avoir les localisations des différentes cellules adjacentes possibles, pour ce déplacer
  def adjacentCell(): Array[Position] = {
    var headX: Int = getHeadPos.x
    var headY: Int = getHeadPos.y

    val listAdjacentCell: ArrayBuffer[Position] = new ArrayBuffer[Position]()

    for (i: Int <- -1 to 1; if (i != 0)) {

      val headYC: Int = stayInTheGrid(headY + i, 'C')
      val headXC: Int = stayInTheGrid(headX + i, 'L')

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

  // Fonction pour faire le mouvement réalisé avec la position donné d'une cellule adjacente
  def moveWithAdjacentCell(pos: Position): Unit = {

    val headX: Int = getHeadPos.x
    val headY: Int = getHeadPos.y


    val xMove: Int = pos.x - headX
    val yMove: Int = pos.y - headY

    // 1er Mouvement
    if (yMove != 0 || xMove != 0) {
      if (yMove == 0) {
        if (xMove < 0) {
          move(West())
        }
        else {
          move(East())
        }
      }
      else {
        if (yMove < 0) {
          move(North())
        }
        else {
          move(South())
        }
      }
    }
  }

  // Fonction qui permet de remplacer un chiffre négatif, par 0. Utilisé dans la fonction adjacentCell(), pour ne pas avoir de coordonnée négative
  def notNegative(nb: Int): Int = {
    if (nb >= 0) {
      return nb
    }
    else return 0
  }

  def stayInTheGrid(nb: Int, typ: Char): Int = {
    var sup: Int = 0
    var nb1: Int = notNegative(nb)

    if (typ == 'C') {
      sup = grid.length
    }
    else {
      sup = grid(0).length
    }

    if (nb >= sup) {
      return sup - 1
    }
    else return nb1
  }

  // Fonction pour finir de remplir la grille avec des obstacles ( 0 remplacé par un obstacle)
  def fillGridWith(what: String = "Obstacle"): Unit = {
    for (lign: Int <- grid.indices) {
      for (column: Int <- grid(lign).indices) {
        val value: Cellule = grid(lign)(column)

        what match {
          case "Obstacle" => {
            if (value.getValueInt == 0) {
              grid(lign)(column).isObstacl = true
            }
          }

          case "Vide" => {
            {
              grid(lign)(column).isObstacl = false
              grid(lign)(column).setValueInt(0)
            }
          }


        }

      }
    }


  }


  // Initialiser la postion de départ
  private val posDepartRandom: Position = new Position((math.random() * grid(0).length - 1).toInt, (math.random() * grid.length - 1).toInt)

  def generateGrid(percentageCoverGrid: Double = 0.8, nbObstacleInit: Int = 0, posDepart: Position = posDepartRandom): Boolean = {


    // Initialisiation sur la grille
    grid(posDepart.y)(posDepart.x).setValueInt(1)


    // création d'obstacles imposés
    var counterObstacles: Int = 0

    if (nbObstacleInit > 0) {
      while (counterObstacles < nbObstacleInit) {
        val posXObs: Int = (math.random() * grid(0).length - 1).toInt
        val posYObs: Int = (math.random() * grid.length - 1).toInt


        val value: Cellule = grid(posYObs)(posXObs)

        if (!value.isObstacl && value.getValueInt < 1) {

          grid(posYObs)(posXObs).isObstacl = true

          counterObstacles += 1
        }
      }
    }


    // Initialistion direction de départ
    val initialAvailableCells: Array[Position] = adjacentCell()
    if (initialAvailableCells.length != 0) {
      val randomNb: Int = (math.random * (initialAvailableCells.length - 1)).toInt
      val initialMove: Position = initialAvailableCells(randomNb)

      // 1er Mouvement
      moveWithAdjacentCell(initialMove)
    }


    def find(): Boolean = {


      // Trouver les nouvelles cellules adjacentes après le 1er mouvement
      val nextAvailableCells: Array[Position] = adjacentCell()

      if (occupation() > percentageCoverGrid && nextAvailableCells.length == 0) {
        return true
      }
      else if (nextAvailableCells.length == 0) {
        return false
      }
      else {
        for (candidat: Position <- nextAvailableCells) {

          val saveGrid: ArrayBuffer[ArrayBuffer[Cellule]] = grid.clone()

          moveWithAdjacentCell(candidat)

          if (find()) {
            return true
          }

          else {
            grid = saveGrid
            return false
          }

        }

        return false
      }
    }

    if (find()) {
      println(s"Rempli à: ${(occupation() * 100).toInt} %")
      fillGridWith()
      return true
    }
    else {

      //New Grid
      fillGridWith("Vide")
      // Initialiser une nouvelle position de départ
      val newDepartRandom: Position = new Position((math.random() * grid(0).length - 1).toInt, (math.random() * grid.length - 1).toInt)

      //Appel récursif pour essayer une nouvelle postion de départ
      if (generateGrid(percentageCoverGrid, nbObstacleInit, newDepartRandom)) {
        return true
      }

      else {
        return false
      }


    }
  }

}