package classes

import scala.collection.mutable.ArrayBuffer

object test extends App {
  var gridTest: Grid = new Grid(10, 9)



  println("----------------------------------------------------------------------")




  println(gridTest.generateGrid())
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
  // grid(1)(0).setValueInt(1)


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

  def headCanMove(): Boolean = {
    var headX: Int = getHeadPos.x
    var headY: Int = getHeadPos.y

    var actions: Array[Direction] = Array(North(), South(), East(), West())

    for (action <- actions) {
      if (!(action.actionX + headX >= grid(0).length || action.actionX + headX < 0 || action.actionY + headY >= grid.length || action.actionY + headY < 0)) {
        if (grid(headY + action.actionY)(headX + action.actionX).isObstacl || grid(headY + action.actionY)(headX + action.actionX).haveSquatter)
          return false
      }
    }
    true
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
    var headX: Int = getHeadPos.x
    var headY: Int = getHeadPos.y

    if (action.actionX + headX >= grid(0).length || action.actionX + headX < 0 || action.actionY + headY >= grid.length || action.actionY + headY < 0) {
      return
    }

    else if (grid(headY + action.actionY)(headX + action.actionX).isObstacl || grid(headY + action.actionY)(headX + action.actionX).haveSquatter) {
      return
    }
    else {
      grid(headY + action.actionY)(headX + action.actionX).setValueInt(grid(headY)(headX).getValueInt + 1)
      move(action)
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
    var counter: Int = 0

    for (lign: Int <- grid.indices) {
      for (column: Int <- grid(lign).indices) {
        val value: Int = grid(lign)(column).getValueInt

        if (value == 0) {
          counter += 1
        }
      }
    }
    return 1 - counter / (grid.length * grid(0).length)
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
    println(s"cell adja disp : ${listAdjacentCell.mkString(",")}")
    return listAdjacentCell.toArray
  }

  // Fonction pour faire le mouvement réalisé avec la position donné d'une cellule adjacente
  def moveWithAdjacentCell(pos: Position): Unit = {

    val headX: Int = getHeadPos.x
    val headY: Int = getHeadPos.y
    println(s"xHead $headX et yHead $headY")

    val xMove: Int = pos.x - headX
    val yMove: Int = pos.y - headY

    println(s"xMove $xMove et yMove $yMove")

    // 1er Mouvement
    if (yMove != 0 || xMove != 0) {
      if (yMove == 0) {
        if (xMove < 0) {
          move(West())
          println("West")
        }
        else {
          move(East())
          println("East")
        }
      }
      else {
        if (yMove < 0) {
          move(North())
          println("North")
        }
        else {
          move(South())
          println("South")
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


  def generateGrid(): Boolean = {


    // Initialiser la postion de départ
    val posDepart: Position = new Position((math.random() * grid.length - 1).toInt, (math.random() * grid.length - 1).toInt)

    // Initialisiation sur la grille
    grid(posDepart.y)(posDepart.x).setValueInt(1)

    // Initialistion direction de départ
    val initialAvailableCells: Array[Position] = adjacentCell()
    val randomNb: Int = (math.random * (initialAvailableCells.length - 1)).toInt
    val initialMove: Position = initialAvailableCells(randomNb)


    // 1er Mouvement
    moveWithAdjacentCell(initialMove)


    def find(): Boolean = {


      // Trouver les nouvelles cellules adjacentes après le 1er mouvement
      val nextAvailableCells: Array[Position] = adjacentCell()

      if (occupation() > 0.8 && nextAvailableCells.length == 0) {
        return true
      }
      else if (nextAvailableCells.length == 0) {
        return false
      }
      else {
        for (candidat: Position <- nextAvailableCells) {

          val saveGrid: ArrayBuffer[ArrayBuffer[Cellule]] = grid.clone()

          moveWithAdjacentCell(candidat)


          display()
          println("--------------------------------------------------------")

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

    return find()

  }


}