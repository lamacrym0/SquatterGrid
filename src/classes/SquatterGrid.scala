package classes

import ch.hevs.gdx2d.desktop.PortableApplication
import ch.hevs.gdx2d.lib.GdxGraphics
import com.badlogic.gdx.Input
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.ui.TextButton

import scala.collection.mutable.ArrayBuffer


object application extends App{
  var game :SquatterGrid = new SquatterGrid()
  game.stratCmdGame(3,3,0)

}

class SquatterGrid() extends PortableApplication(1920,1080){
  var nbLvl:Int = 1
  var grid:Grid = null
  var line:Int = 0
  var column:Int = 0
  var obsacl:Int = 0
  var TheGrid: GridValid = new GridValid(false,null,null)
  var solutionVisible : Boolean = false
  //var newGameButton

  def restartCmdGame(): Unit = {
    println(s"taille: $line x $column, obstacles: $obsacl")
    grid.resetGrid()
    grid.display()
  }

  def stratCmdGame(line: Int, column: Int,obstacl:Int): Unit = {
    this.line = line
    this.column = column
    println(s"taille: $line x $column, obstacles: $obsacl")
    grid = new Grid(line, column)

    var isGenerer: Boolean = false
    while (!isGenerer) {
      try {
        val miniArea : Double = 0.5
        var area : Double =  math.random
        if(area < miniArea){
          area = miniArea
        }
        val miniObs : Int = 1
        val maxObs : Int = (line * column * 0.1).toInt
        var nbObs : Int =  (math.random * maxObs).toInt
        if(nbObs<miniObs){
          nbObs = miniObs
        }

        grid.generateGrid(percentageCoverGrid = area, nbObstacleInit = nbObs)

        isGenerer = true
        Grids.addOne(grid)
      }
    }

    grid.display()
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
          println(!grid.gridIsFinish)

          if (grid.gridIsFinish) {
            if(nbLvl % 6 == 0){
              line += 1
              nbLvl = 0
              obsacl = 0
            } else if( nbLvl % 4 == 0){
              column += 1
            }
            if(nbLvl %2 ==0)
            obsacl += 1

            println(nbLvl)
            stratCmdGame(line,column,obsacl)
          }else {
            restartCmdGame()
          }

        }

        // Touche pour voir la solution dans la console de la grille
      case Input.Keys.S => TheGrid.display(grid.gridSolution)
      if(!solutionVisible){
        solutionVisible = true
      }
      else solutionVisible = false

      case _ =>
    }
  }
  def actionKeyInput(action:Direction):Unit = {
    if (grid.move(action) != 0) {
      grid.display()
      controlStatGame()
    }
  }

  def controlStatGame(): Unit = {
    if(!grid.headCanMove()){
      if (grid.gridIsFinish) {
        println("You Win! \n press space to go to the next level.")
        nbLvl += 1
      }
      else{
        println("You lose :( \n press space to restart.")
      }
    }
  }
  override def onGraphicRender(g: GdxGraphics): Unit = {

    g.clear()
    g.setBackgroundColor(Color.valueOf("48d055"))
    g.drawFPS()
    g.drawSchoolLogo()
    grid.displayWin(g)

  }
}
