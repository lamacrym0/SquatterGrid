package classes

import ch.hevs.gdx2d.components.bitmaps.Spritesheet
import ch.hevs.gdx2d.desktop.PortableApplication
import ch.hevs.gdx2d.lib.GdxGraphics
import com.badlogic.gdx.Input
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.BitmapFont
import scala.collection.mutable.ArrayBuffer

object application extends App {
  val game: SquatterGrid = new SquatterGrid()
  game.startGame(4,4)
}

class SquatterGrid() extends PortableApplication(1920, 1200) {
  private var haveMove: Boolean = false
  private var nbLvl: Int = 1
  private var line: Int = 0
  private var column: Int = 0
  private var isShaking: Boolean = false
  private var nbShake: Int = 0
  private var solutionVisible: Boolean = false

  var grid: Grid = null
  var catSs: Spritesheet = null
  var action: Direction = null
  var oldGrid: Grid = null
  var nbMove: Int = 0
  var x: Int = 0
  var y: Int = 0

  def startGame(line: Int, column: Int): Unit = {
    this.line = line
    this.column = column
    grid = new Grid(line, column)

    val miniArea: Double = 0.5
    var area: Double = math.random
    if (area < miniArea)
      area = miniArea

    val miniObs: Int = 1
    val maxObs: Int = (line * column * 0.1).toInt
    var nbObs: Int = (math.random * maxObs).toInt
    if (nbObs < miniObs)
      nbObs = miniObs

    grid.generateGrid(percentageCoverGrid = area, nbObstacleInit = nbObs)
  }

  override def onInit(): Unit = {
    setTitle("Squatter Grid")
  }

  override def onKeyDown(keycode: Int): Unit = {

    keycode match {
      case Input.Keys.DOWN =>
        actionKeyInput(South())
      case Input.Keys.UP =>
        actionKeyInput(North())
      case Input.Keys.LEFT =>
        actionKeyInput(West())
      case Input.Keys.RIGHT =>
        actionKeyInput(East())

      case Input.Keys.SPACE =>
          if (!grid.headCanMove()) {
            if (grid.gridIsFinish) {
              if (nbLvl == 50) {
                line = 4
                column = 4
                nbLvl = 0
              }
              if (nbLvl % 10 == 0)
                column += 1
              else if (nbLvl % 5 == 0)
                line += 1
              nbLvl += 1

            startGame(line, column)
          }
          else
            grid.resetGrid()
        }

      // Touche pour voir la solution dans la console de la grille
      case Input.Keys.S => grid.display(grid.gridSolution, true)
        solutionVisible = !solutionVisible
      if(!solutionVisible)
        grid.gridSolutionString = ""

      case Input.Keys.A => automaticSolver()
      case _ =>
    }
  }

  private def automaticSolver(): Unit = {

    val positionHead: Position = grid.getHeadPos
    val valueHeadInt: Int = grid.gridSolution(positionHead.y)(positionHead.x).getValueInt

    val cellavailable: Array[Position] = grid.adjacentCell()

    if (!(cellavailable.isEmpty && grid.gridIsFinish)) {

      for (pos: Int <- cellavailable.indices) {
        val cellTest: Position = cellavailable(pos)
        if (grid.gridSolution(cellTest.y)(cellTest.x).getValueInt == (valueHeadInt + 1)) {
          val xMove: Int = cellTest.x - positionHead.x
          val yMove: Int = cellTest.y - positionHead.y

          // 1er Mouvement
          if (yMove != 0 || xMove != 0) {
            if (yMove == 0) {
              if (xMove < 0)
                actionKeyInput(West())
              else
                actionKeyInput(East())
            }
            else {
              if (yMove < 0)
                actionKeyInput(North())
              else
                actionKeyInput(South())
            }
          }
        }
      }
    }
  }

  private def actionKeyInput(action: Direction): Unit = {

    if (haveMove)
      return
    oldGrid = new Grid(grid.grid(0).length, grid.grid.length)
    for (y <- grid.grid.indices; x <- grid.grid(y).indices)
      oldGrid.grid(y)(x) = new Cellule(grid.grid(y)(x).isObstacl, grid.grid(y)(x).haveSquatter, grid.grid(y)(x).getValueInt)

    nbMove = grid.move(action)

    if (nbMove != 0) {
      haveMove = true
      this.action = action
      controlStatGame()
    }
  }

  private def controlStatGame(): Unit = {
    if (!grid.headCanMove()) {
      if (grid.gridIsFinish)
        println("You Win! \n press space to go to the next level.")
      else
        println("You lose :( \n press space to restart.")
    }
  }

  private def shake(g: GdxGraphics, nbShake: Int, in: Grid = grid): Boolean = {

    g.drawString(60,400,grid.gridSolutionString, new BitmapFont(),45)
    if (this.nbShake >= nbShake) {
      in.displayWin(g, catSs = catSs)
      return true
    }

    val width: Int = g.getScreenHeight / (in.grid.length + 2)
    val xStart: Int = g.getScreenWidth / 2 - width * in.grid.length / 2 + width / 2
    val yStart: Int = width + width / 2
    val headPos: Position = in.getHeadPos

    val posShake: Int = this.nbShake match {
      case 1 | 2 | 3 => 10
      case 4 | 5 | 6 => 0
      case 7 | 8 | 9 => -10
      case _ => 0
    }

    for (y <- in.grid.indices; x <- in.grid(y).indices) {
      if (y == headPos.y && x == headPos.x)
        g.draw(catSs.sprites(0)(0), (posShake + xStart + x * width - width / 2).toFloat, (g.getScreenHeight - (posShake + yStart + y * width) - width / 2).toFloat, width.toFloat, width.toFloat)
      else if (in.grid(y)(x).isObstacl)
        g.drawFilledRectangle(posShake + xStart + x * width, g.getScreenHeight - (posShake + yStart + y * width), width, width, 0, Color.valueOf("43cf64"))
      else if (in.grid(y)(x).getValueInt > 0)
        g.drawFilledRectangle(posShake + xStart + x * width, g.getScreenHeight - (posShake + yStart + y * width), width, width, 0, Color.valueOf("ffeb00"))
      else
        g.drawFilledRectangle(posShake + xStart + x * width, g.getScreenHeight - (posShake + yStart + y * width), width, width, 0, Color.valueOf("e1755a"))
    }
    this.nbShake += 1

    false
  }

  override def onGraphicRender(g: GdxGraphics): Unit = {

    g.clear()

    if (catSs == null) {
      catSs = new Spritesheet("data/images/CHAT.png", 8, 8)
    }

    g.setBackgroundColor(Color.valueOf("43cf64"))

    if (haveMove) {
      val width: Int = g.getScreenHeight / (grid.grid.length + 2)
      x += action.actionX * width / 5
      y += action.actionY * width / 5

      haveMove = !grid.displayMove(g, oldGrid, action, x, y, nbMove, width, catSs)
      if (!haveMove) {
        isShaking = true
        this.nbShake = 0
      }
    } else if (isShaking) {
      isShaking = !shake(g, 10)
    }
    else {
      grid.displayWin(g, catSs = catSs)
    }
    if (!haveMove) {
      x = 0
      y = 0
    }
    g.drawFPS()
    g.drawSchoolLogo()
  }
}
