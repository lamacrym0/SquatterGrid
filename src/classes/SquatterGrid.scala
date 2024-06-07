package classes

import ch.hevs.gdx2d.components.bitmaps.Spritesheet
import ch.hevs.gdx2d.desktop.PortableApplication
import ch.hevs.gdx2d.lib.GdxGraphics
import ch.hevs.gdx2d.lib.utils.Logger
import com.badlogic.gdx.{Gdx, Input}
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.{InputEvent, Stage}
import com.badlogic.gdx.scenes.scene2d.ui.{Skin, TextButton, TextField}
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object application extends App {
  var game: SquatterGrid = new SquatterGrid()
  game.stratCmdGame(3, 3, 0)

}

class SquatterGrid() extends PortableApplication(1920, 1200) {

  var nbLvl: Int = 1
  var grid: Grid = null
  var line: Int = 0
  var column: Int = 0
  var obsacl: Int = 0

  var action: Direction = null
  var haveMove: Boolean = false
  var oldGrid: Grid = null
  var nbMove: Int = 0
  var menu: Boolean = true
  var isShaking :Boolean = false
  var nbShake:Int = 0

  var catSs: Spritesheet = null

  var solutionVisible: Boolean = false
  //var newGameButton

  def restartCmdGame(): Unit = {
    println(s"taille: $line x $column, obstacles: $obsacl")
    grid.resetGrid()
    grid.display()
  }

  def stratCmdGame(line: Int, column: Int, obstacl: Int): Unit = {
    this.line = line
    this.column = column
    println(s"taille: $line x $column, obstacles: $obsacl")
    grid = new Grid(line, column)

    var isGenerer: Boolean = false
    while (!isGenerer) {
      try {
        val miniArea: Double = 0.5
        var area: Double = math.random
        if (area < miniArea) {
          area = miniArea
        }
        val miniObs: Int = 1
        val maxObs: Int = (line * column * 0.1).toInt
        var nbObs: Int = (math.random * maxObs).toInt
        if (nbObs < miniObs) {
          nbObs = miniObs
        }

        grid.generateGrid(percentageCoverGrid = area, nbObstacleInit = nbObs)

        isGenerer = true

      }
    }

    grid.display()
  }

  var stage: Stage = null

  override def onInit(): Unit = {
    setTitle("Squatter Grid")
    stage = new Stage

  }

  override def onKeyDown(keycode: Int): Unit = {
    print("ok")
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
            if (nbLvl % 6 == 0) {
              line += 1
              nbLvl = 0
              obsacl = 0
            } else if (nbLvl % 4 == 0) {
              column += 1
            }
            if (nbLvl % 2 == 0) {
              obsacl += 1
            }
            nbLvl+=1

            stratCmdGame(line, column, obsacl)
          } else {
            restartCmdGame()
          }

        }

      // Touche pour voir la solution dans la console de la grille
      case Input.Keys.S => grid.display(grid.gridSolution)
        if (!solutionVisible) {
          solutionVisible = true
        }
        else solutionVisible = false


      case _ =>
    }
  }

  def actionKeyInput(action: Direction): Unit = {

    if (haveMove || isShaking) {
      return
    }

    oldGrid = new Grid(grid.grid(0).length, grid.grid.length)
    for (y <- grid.grid.indices; x <- grid.grid(y).indices) {
      oldGrid.grid(y)(x) = new Cellule(grid.grid(y)(x).isObstacl, grid.grid(y)(x).haveSquatter, grid.grid(y)(x).getValueInt)
    }


    nbMove = grid.move(action)

    if (nbMove != 0) {
      haveMove = true
      this.action = action
      grid.display()
      controlStatGame()
    }

  }


  def controlStatGame(): Unit = {
    if (!grid.headCanMove()) {
      if (grid.gridIsFinish) {
        println("You Win! \n press space to go to the next level.")
        nbLvl += 1
      }
      else{
        println("You lose :( \n press space to restart.")
      }
    }
  }


  def shake(g:GdxGraphics, nbShake: Int,in:Grid = grid): Boolean = {
    if (this.nbShake >= nbShake) {
      in.displayWin(g,catSs = catSs)
      return true
    }

    val width: Int = g.getScreenHeight / (in.grid.length + 2)
    val xStart: Int = g.getScreenWidth / 2 - width * in.grid.length / 2 + width / 2
    val yStart: Int = width + width / 2
    val headPos: Position = in.getHeadPos

    val posShake:Int = this.nbShake match {
      case (1| 2 | 3)=> 10
      case (4| 5 | 6) => 0
      case (7| 8 | 9) => -10
      case _ => 0
    }

    for (y <- in.grid.indices; x <- in.grid(y).indices) {
      if (y == headPos.y && x == headPos.x) {
        g.draw(catSs.sprites(0)(0), (posShake + xStart + x * width - width / 2).toFloat, (g.getScreenHeight - (posShake + yStart + y * width) - width / 2).toFloat, width.toFloat, width.toFloat)
      }
      else if (in.grid(y)(x).isObstacl) {
        g.drawFilledRectangle(posShake + xStart + x * width, g.getScreenHeight - (posShake + yStart + y * width), width, width, 0, Color.valueOf("48d055"))
      } else if (in.grid(y)(x).getValueInt > 0) {
        g.drawFilledRectangle(posShake + xStart + x * width, g.getScreenHeight - (posShake + yStart + y * width), width, width, 0, Color.valueOf("ffeb00"))
      } else {
        g.drawFilledRectangle(posShake + xStart + x * width, g.getScreenHeight - (posShake + yStart + y * width), width, width, 0, Color.valueOf("e1755a"))
      }
    }
    this.nbShake += 1
    print("shake")
    false
  }
  var x:Int = 0
  var y:Int = 0

  override def onGraphicRender(g: GdxGraphics): Unit = {

    g.clear()

    if(catSs == null){
     catSs = new Spritesheet("data/images/CHAT.png",8,8)
    }

    g.setBackgroundColor(Color.valueOf("48d055"))

    if (haveMove) {
      val width: Int = g.getScreenHeight / (grid.grid.length + 2)
      x += action.actionX * width / 5
      y += action.actionY * width / 5

      haveMove = !grid.displayMove(g, oldGrid, action, x, y, nbMove, width,catSs)
      if(!haveMove){
        isShaking = true
        this.nbShake = 0
      }
    } else if(isShaking){
      isShaking = !shake(g,10)
    }
    else {
      grid.displayWin(g,catSs = catSs)
    }
    if (!haveMove) {

      x = 0
      y = 0
    }
    g.drawFPS()
    g.drawSchoolLogo()

  }

}
