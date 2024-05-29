package classes

import ch.hevs.gdx2d.components.bitmaps.BitmapImage
import ch.hevs.gdx2d.desktop.PortableApplication
import ch.hevs.gdx2d.lib.GdxGraphics
import com.badlogic.gdx.Input

object application extends App{
  var game :SquatterGrid = new SquatterGrid()
  game.stratCmdGame(3,3)
}

class SquatterGrid() extends PortableApplication{
  var grid:Grid = null
  var line:Int = 0
  var column:Int = 0

  def stratCmdGame(line:Int,column: Int): Unit = {
    this.line = line
    this.column = column
    grid = new Grid(line,column)
    grid.display()
  }
  // lancer la partie
  // demander le move
  // effectuer le move
  // controler si la grille et fini
  /// si oui continuer
  /// sinon revenir à demander le move

  override def onInit(): Unit = {
    setTitle("Hello World - mui 2024")
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
      case Input.Keys.SPACE =>{
        if (!grid.headCanMove()) {
          print(!grid.headCanMove())
          stratCmdGame(line,column)
        }
      }
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
        println("You Win! \n press space to restart.")
      }
      else{
        println("You lose :( \n press space to restart.")
      }
    }
  }
  override def onGraphicRender(g: GdxGraphics): Unit = {
    // Clears the screen
    g.clear()
    g.drawStringCentered(getWindowHeight * 0.8f, "Welcome to gdx2d !")
    g.drawFPS()
    g.drawSchoolLogo()
  }
}
