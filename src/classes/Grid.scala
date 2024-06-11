package classes
import ch.hevs.gdx2d.components.bitmaps.Spritesheet
import ch.hevs.gdx2d.lib.GdxGraphics
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.BitmapFont
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Grid(x: Int, y: Int) extends Serializable {

  var grid:ArrayBuffer[ArrayBuffer[Cellule]] = ArrayBuffer()
  var gridSolution: ArrayBuffer[ArrayBuffer[Cellule]] = ArrayBuffer()
  var gridSolutionString :String = ""
  private var catAnim:Int = 0

  for (idy <- 0 until y) {
    grid.addOne(new ArrayBuffer[Cellule]())
    for (_ <- 0 until x) {
      grid(idy).addOne(new Cellule())
    }
  }

  def resetGrid(): Unit = {
    for (y <- grid.indices; x <- grid(y).indices if grid(y)(x).getValueInt != 0 && grid(y)(x).getValueInt != 1) {
      grid(y)(x).setValueInt(0)
    }
  }

  def displayMove(g:GdxGraphics,oldGrid:Grid,action:Direction,x:Int,y:Int,nbMove:Int,width:Int,catSs:Spritesheet):Boolean = {

    val xStart: Int = g.getScreenWidth / 2 - width * oldGrid.grid.length / 2 + width / 2
    val yStart: Int = width + width / 2

    displayWin(g,oldGrid,catSs)

    if(action.isInstanceOf[South] || action.isInstanceOf[East]){
      if (x >= nbMove * width || y >= nbMove * width) {
        displayWin(g,catSs = catSs)
        return true
      }

      if(action.isInstanceOf[South]){
        val headPos:Position = oldGrid.getHeadPos
        g.drawFilledRectangle(xStart + headPos.x * width,  g.getScreenHeight - width/2 -((y/2 )+ yStart + (headPos.y-1) * width),width,y,0,Color.valueOf("ffeb00"))
        g.drawFilledRectangle(xStart + headPos.x * width,  g.getScreenHeight - (yStart + headPos.y * width),width,width,0,Color.valueOf("ffeb00"))
        g.draw(catSs.sprites(0)(0),(xStart + headPos.x * width - width/2).toFloat,  (g.getScreenHeight - (y+yStart + headPos.y * width)- width/2).toFloat,width.toFloat,width.toFloat)
      } else {
        val headPos: Position = oldGrid.getHeadPos
        g.drawFilledRectangle(xStart + headPos.x * width,  g.getScreenHeight - (yStart + headPos.y * width),width,width,0,Color.valueOf("ffeb00"))
        g.drawFilledRectangle(xStart + headPos.x * width + x/2 +width/2, g.getScreenHeight - (yStart + headPos.y * width), x, width, 0, Color.valueOf("ffeb00"))
        g.draw(catSs.sprites(0)(0),(xStart + headPos.x * width  + x - width/2).toFloat, (g.getScreenHeight - (yStart + headPos.y * width)-width/2).toFloat,width.toFloat,width.toFloat)
      }

    }else{
      if (-x >= nbMove * width || -y >= nbMove * width) {
        displayWin(g,catSs = catSs)
        return true

      }
      if(action.isInstanceOf[North]) {
        val headPos: Position = oldGrid.getHeadPos
        g.drawFilledRectangle(xStart + headPos.x * width, g.getScreenHeight - y / 2 +  width / 2 - ( + yStart + headPos.y * width), width, -y, 0, Color.valueOf("ffeb00"))
        g.drawFilledRectangle(xStart + headPos.x * width,  g.getScreenHeight - (yStart + headPos.y * width),width,width,0,Color.valueOf("ffeb00"))
        g.draw(catSs.sprites(0)(0),(xStart + headPos.x * width - width/2).toFloat,  (g.getScreenHeight - (y+yStart + headPos.y * width)- width/2).toFloat,width.toFloat,width.toFloat)
      } else {
        val headPos: Position = oldGrid.getHeadPos
        g.drawFilledRectangle(xStart + headPos.x * width + x / 2 - width / 2, g.getScreenHeight - (yStart + headPos.y * width), -x, width, 0, Color.valueOf("ffeb00"))
        g.drawFilledRectangle(xStart + headPos.x * width,  g.getScreenHeight - (yStart + headPos.y * width),width,width,0,Color.valueOf("ffeb00"))
        g.draw(catSs.sprites(0)(0),(xStart + headPos.x * width + x - width/2).toFloat , (g.getScreenHeight - (yStart + headPos.y * width)-width/2).toFloat,width.toFloat,width.toFloat)
      }
    }
    false
  }

  def displayWin(g: GdxGraphics, in: Grid = this,catSs:Spritesheet): Unit = {

    g.drawString(60,400,gridSolutionString, new BitmapFont(),45)

    if(catAnim > 200){
      catAnim = 0
    }
    val width: Int = g.getScreenHeight / (in.grid.length + 2)
    val xStart: Int = g.getScreenWidth / 2 - width * in.grid.length /2  + width/2
    val yStart: Int = width + width / 2
    val headPos:Position = in.getHeadPos

    for (y <- in.grid.indices; x <- in.grid(y).indices) {
      if(y == headPos.y && x == headPos.x){
        if(catAnim == 100 && catAnim < 102){
          g.draw(catSs.sprites(0)(1),(xStart + x * width - width/2).toFloat, (g.getScreenHeight -  (yStart + y * width)- width/2).toFloat,width.toFloat,width.toFloat)

        } else if(catAnim >= 102 && catAnim < 110){
          g.draw(catSs.sprites(0)(2),(xStart + x * width - width/2).toFloat, (g.getScreenHeight -  (yStart + y * width)- width/2).toFloat,width.toFloat,width.toFloat)

        } else if(catAnim >= 110 && catAnim < 120){
          g.draw(catSs.sprites(0)(3),(xStart + x * width - width/2).toFloat, (g.getScreenHeight -  (yStart + y * width)- width/2).toFloat,width.toFloat,width.toFloat)

        } else {
          g.draw(catSs.sprites(0)(0),(xStart + x * width - width/2).toFloat, (g.getScreenHeight -  (yStart + y * width)- width/2).toFloat,width.toFloat,width.toFloat)
        }
      }
      else if (in.grid(y)(x).isObstacl) {
        g.drawFilledRectangle(xStart + x * width, g.getScreenHeight - (yStart + y * width), width, width, 0, Color.valueOf("48d055"))
      } else if (in.grid(y)(x).getValueInt > 0) {
        g.drawFilledRectangle(xStart + x * width, g.getScreenHeight -  (yStart + y * width), width, width, 0, Color.valueOf("ffeb00"))
      } else {
        g.drawFilledRectangle(xStart + x * width, g.getScreenHeight - (yStart + y * width), width, width, 0, Color.valueOf("e1755a"))
      }
    }
    catAnim += 1
  }

  def display(gridIn: ArrayBuffer[ArrayBuffer[Cellule]] = grid,visible : Boolean = false): Unit = {
    var ligne : String = ""
    for(_ <- gridIn){
      ligne += "-----"
    }
    print("\r")
    var res: String = ""
    for (y <- gridIn.indices) {
      for (x <- gridIn(y).indices) {
        if (!gridIn(y)(x).isObstacl) {
          if (gridIn(y)(x).haveSquatter) {
            val value : Int = gridIn(y)(x).getValueInt
            if (value <= 9){
              res += "|" + value + "  |"
            }
            else {
              res += "|" + value + " |"
            }
          } else {
            res += "| 0 |"
          }
        }
        else {
          res += "| X |"
        }
      }
      res += "\n"
      res += ligne
      res += "\n"
    }
    if(visible){
      gridSolutionString = "---------Solution---------\n\n"+res
    }
    print(res)
  }

  def headCanMove(): Boolean = {

    val headX: Int = getHeadPos.x
    val headY: Int = getHeadPos.y
    val actions: Array[Direction] = Array(North(), South(), East(), West())

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
    for (y <- grid.indices; x <- grid(y).indices if grid(y)(x).getValueInt == 0 && !grid(y)(x).isObstacl) {
      return false
    }
    true
  }

  def move(action: Direction, inv : Boolean = false): Int = {
    if (inv) {
      moveInv(action)
    }
    else {
      val headX: Int = getHeadPos.x
      val headY: Int = getHeadPos.y

      if (action.actionX + headX >= grid(0).length || action.actionX + headX < 0 || action.actionY + headY >= grid.length || action.actionY + headY < 0)
        0
      else if (grid(headY + action.actionY)(headX + action.actionX).isObstacl || grid(headY + action.actionY)(headX + action.actionX).haveSquatter)
        0
      else {
        grid(headY + action.actionY)(headX + action.actionX).setValueInt(grid(headY)(headX).getValueInt + 1)
        1 + move(action)
      }
    }
  }

  private def moveInv(action: Direction): Int = {
    val headX: Int = getHeadPos.x
    val headY: Int = getHeadPos.y

    if (action.actionX + headX >= grid(0).length || action.actionX + headX < 0 || action.actionY + headY >= grid.length || action.actionY + headY < 0)
      0
    else if (grid(headY + action.actionY)(headX + action.actionX).isObstacl || !grid(headY + action.actionY)(headX + action.actionX).haveSquatter)
      0
    else {
      grid(headY + action.actionY)(headX + action.actionX).setValueInt(0)
      1 + move(action)
    }
  }

  def getHeadPos: Position = {
    var res: Position = new Position(0, 0)
    var maxVal: Int = 0
    for (y <- grid.indices) {
      for (x <- grid(y).indices if grid(y)(x).getValueInt > maxVal) {
        maxVal = grid(y)(x).getValueInt
        res = new Position(x, y)
      }
    }
    res
  }

  // Fonction pour calculer le pourcentage d'occupation de la surface par le "Squatter"
  private def occupation(): Double = {
    var counter: Double = 0
    var counterObstacle: Double = 0
    val surfaceInit: Double = grid.length * grid(0).length

    for (lign: Int <- grid.indices) {
      for (column: Int <- grid(lign).indices) {
        val value: Cellule = grid(lign)(column)

        if (value.getValueInt != 0 && !value.isObstacl)
          counter += 1.0
        else if (value.isObstacl)
          counterObstacle += 1
      }
    }
    counter / (surfaceInit - counterObstacle)
  }

  // Fonction pour avoir les localisations des différentes cellules adjacentes possibles, pour ce déplacer
  def adjacentCell(): Array[Position] = {
    val headX: Int = getHeadPos.x
    val headY: Int = getHeadPos.y

    val listAdjacentCell: ArrayBuffer[Position] = new ArrayBuffer[Position]()

    for (i: Int <- -1 to 1; if i != 0) {

      val headYC: Int = stayInTheGrid(headY + i, 'C')
      val headXC: Int = stayInTheGrid(headX + i, 'L')

      val posUD: Position = new Position(headX, headYC)
      val posRL: Position = new Position(headXC, headY)

      val moveRL: Cellule = grid(posRL.y)(posRL.x)
      val moveUD: Cellule = grid(posUD.y)(posUD.x)

      if (!moveRL.haveSquatter && !moveRL.isObstacl && headX + i == headXC)
        listAdjacentCell.addOne(posRL)
      if (!moveUD.haveSquatter && !moveUD.isObstacl && headY + i == headYC)
        listAdjacentCell.addOne(posUD)
    }

    listAdjacentCell.toArray
  }

  // Fonction pour faire le mouvement réalisé avec la position donné d'une cellule adjacente
  private def moveWithAdjacentCell(pos: Position): Unit = {

    val headX: Int = getHeadPos.x
    val headY: Int = getHeadPos.y
    val xMove: Int = pos.x - headX
    val yMove: Int = pos.y - headY

    // 1er Mouvement
    if (yMove != 0 || xMove != 0) {
      if (yMove == 0) {
        if (xMove < 0)
          move(West())
        else
          move(East())
      }
      else {
        if (yMove < 0)
          move(North())
        else
          move(South())
      }
    }
  }

  // Fonction qui permet de remplacer un chiffre négatif, par 0. Utilisé dans la fonction adjacentCell(), pour ne pas avoir de coordonnée négative
  private def notNegative(nb: Int): Int = if (nb >= 0) nb else 0

  private def stayInTheGrid(nb: Int, typ: Char): Int = {
    var sup: Int = 0
    val nb1: Int = notNegative(nb)

    if (typ == 'C')
      sup = grid.length
    else
      sup = grid(0).length

    if (nb >= sup) sup -1 else nb1
  }

  // Fonction pour finir de remplir la grille avec des obstacles ( 0 remplacé par un obstacle)
  private def fillGridWith(what: String = "Obstacle"): Unit = {
    for (lign: Int <- grid.indices) {
      for (column: Int <- grid(lign).indices) {
        val value: Cellule = grid(lign)(column)

        what match {
          case "Obstacle" =>
            if (value.getValueInt == 0)
              grid(lign)(column).isObstacl = true

          case "Vide" =>
            grid(lign)(column).isObstacl = false
            grid(lign)(column).setValueInt(0)
        }
      }
    }
  }


  // Méthode pour faire une copie de la solution, afin d'éviter le pb avec la methode .clone
  private def cloneGrid(): Unit = {
    for (lign: Int <- grid.indices) {
      val lignArrayB: ArrayBuffer[Cellule] = new ArrayBuffer[Cellule]()
      for (column: Int <- grid(lign).indices) {
        val save: Cellule = grid(lign)(column)
        lignArrayB.addOne(new Cellule(save.isObstacl, save.haveSquatter, save.getValueInt))
      }
      gridSolution.addOne(lignArrayB)
    }
  }

  // Initialiser la postion de départ
  private val posDepartRandom: Position = new Position((math.random() * grid(0).length - 1).toInt, (math.random() * grid.length - 1).toInt)

  def generateGrid(percentageCoverGrid: Double = 0.8, nbObstacleInit: Int = 0, posDepart: Position = posDepartRandom, saveInitialAvailableCellsArray: ArrayBuffer[Position] = ArrayBuffer(), maxDepth: Int = 10000, currentDepth: Int = 0): Boolean = {
    if (currentDepth > maxDepth)
      return false


    // Initialisation sur la grille
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

    // Initialisation direction de départ
    var initialAvailableCells: ArrayBuffer[Position] = adjacentCell().to(ArrayBuffer)

    // Si il reste des cellules adjacentes à tester, si on a fait un appel récursif
    val saveInitialAvailableCellsArrayFinal: ArrayBuffer[Position] = saveInitialAvailableCellsArray.clone

    if (saveInitialAvailableCellsArray.nonEmpty)
      initialAvailableCells = saveInitialAvailableCellsArray


    if (initialAvailableCells.nonEmpty) {
      val randomNb: Int = (math.random * (initialAvailableCells.length - 1)).toInt

      // On sauvegarde les autres cellules adjacentes non utilisées
      if (saveInitialAvailableCellsArrayFinal.nonEmpty)
        saveInitialAvailableCellsArrayFinal.remove(randomNb)


      // 1er Mouvement
      val initialMove: Position = initialAvailableCells(randomNb)
      moveWithAdjacentCell(initialMove)
    }

    def find(): Boolean = {
      // Trouver les nouvelles cellules adjacentes après le 1er mouvement
      // On mélange les solutions pour éviter qu'il tourne toujours de la même manière
      val nextAvailableCells: Array[Position] = Random.shuffle(adjacentCell().toSeq).toArray

      if (occupation() > percentageCoverGrid && nextAvailableCells.isEmpty)
        true
      else if (nextAvailableCells.isEmpty)
        false
      else {
        for (candidat: Position <- nextAvailableCells) {
          val saveGrid: ArrayBuffer[ArrayBuffer[Cellule]] = grid.clone()

          moveWithAdjacentCell(candidat)

          if (find())
            return true
          else {
            grid = saveGrid
            return false
          }
        }
        false
      }
    }

    if (find()) {
      println(s"Rempli à: ${(occupation() * 100).toInt} %")

      // On remplit les trous avec la fonction ci-dessous
      fillGridWith()

      // On sauvegarde la solution
      cloneGrid()

      // On efface le chemin
      resetGrid()

      println("------------------------------")

      true
    } else {
      // Si pas de solution
      if (saveInitialAvailableCellsArrayFinal.nonEmpty) {
        // On efface le chemin testé
        resetGrid()

        // Appel récursif pour essayer une nouvelle direction, avec le même point de départ.
        if (generateGrid(percentageCoverGrid, 0, posDepart, saveInitialAvailableCellsArrayFinal, maxDepth, currentDepth + 1))
          true
        else
          true
      } else {

        // Si toutes les directions pour un même départ ne donnent pas de solution
        // New Grid
        fillGridWith("Vide")

        // Initialiser une nouvelle position de départ
        val newDepartRandom: Position = new Position((math.random() * grid(0).length - 1).toInt, (math.random() * grid.length - 1).toInt)

        // Appel récursif pour essayer une nouvelle position de départ
        if (generateGrid(percentageCoverGrid, nbObstacleInit, newDepartRandom, maxDepth = maxDepth, currentDepth = currentDepth + 1))
          true
        else {
          resetGrid()
          false
        }
      }
    }
  }
}
