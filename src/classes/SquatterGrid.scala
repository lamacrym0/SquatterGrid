package classes

import ch.hevs.gdx2d.desktop.PortableApplication
import ch.hevs.gdx2d.lib.GdxGraphics
import com.badlogic.gdx.Input
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.ui.TextButton


object application extends App{
  var game :SquatterGrid = new SquatterGrid()
  game.stratCmdGame(3,3)
}

class SquatterGrid() extends PortableApplication{
  var nbLvl:Int = 1
  var grid:Grid = null
  var line:Int = 0
  var column:Int = 0
  var obsacl:Int = 0
  //var newGameButton = new

  def stratCmdGame(line:Int,column:Int): Unit = {
    this.line = line
    this.column = column
    println(s"taille: $line x $column, obstacles: $obsacl")
    grid = new Grid(line,column)
    grid.display()
  }

  override def onInit(): Unit = {
    setTitle("Hello World - mui 2024")
   /* newGameButton = new TextButton("Click me", skin) // Use the initialized skin

    newGameButton.setWidth(buttonWidth)
    newGameButton.setHeight(buttonHeight)
*/
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
            if(nbLvl % 6 == 0){
              line += 1
              nbLvl = 0
              obsacl = 0
            } else if( nbLvl % 4 == 0){
              column += 1
            }
            if(nbLvl %2 ==0)
              obsacl += 1


          }else {
            grid.resetGrid()
          }
          stratCmdGame(line,column)
        }

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
  }
}
