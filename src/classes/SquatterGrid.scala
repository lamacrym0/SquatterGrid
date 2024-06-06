package classes

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
  var TheGrid: GridValid = new GridValid(false, null, null)
  var action: Direction = null
  var haveMove: Boolean = false
  var oldGrid: Grid = null
  var nbMove: Int = 0
  var menu: Boolean = true

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
        Grids.addOne(grid)
      }
    }

    grid.display()
  }


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

      case Input.Keys.R => menu = true


      case _ =>
    }
  }

  def actionKeyInput(action: Direction): Unit = {

    if (haveMove) {
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

  var stage: Stage = null

  def menuGame(g: GdxGraphics): Unit = {

    Gdx.input.setInputProcessor(stage) // Make the stage consume events

    // Load the default skin (which can be configured in the JSON file)
    var skin: Skin = new Skin(Gdx.files.internal("data/ui/uiskin.json"))

    def buttoncreate(txt: String, col: Int = 220, lign: Int = 420, sizeW: Int = 200, sizeH: Int = 200, numButton: Int, offset: Int = 0): Boolean = {

      val buttonWidth: Int = sizeW
      val buttonHeight: Int = sizeH

      val newGameButton = new TextButton(s"$txt $numButton", skin) // Use the initialized skin

      newGameButton.setWidth(buttonWidth)
      newGameButton.setHeight(buttonHeight)

      newGameButton.setPosition(lign * (numButton - offset), col)

      stage.addActor(newGameButton)


      // Register listener


      newGameButton.addListener(new ClickListener() {
        override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
          super.clicked(event, x, y)
          if (newGameButton.isChecked) {
            Logger.log(s"Button is checked $numButton")
            grid = Grids(numButton - 1)
            menu = false
          }
          else {
            Logger.log("Button is not checked")

          }
        }
      })

    }


    val lign1: Int = 240

    var button1Select: Boolean = buttoncreate("Niveau", col = lign1, numButton = 1)

    var button2Select: Boolean = buttoncreate("Niveau", col = lign1, numButton = 2)

    var button3Select: Boolean = buttoncreate("Niveau", col = lign1, numButton = 3)

    val lign2: Int = 620

    var button4Select: Boolean = buttoncreate("Niveau", col = lign2, numButton = 4, offset = 3)

    var button5Select: Boolean = buttoncreate("Niveau", col = lign2, numButton = 5, offset = 3)

    var button6Select: Boolean = buttoncreate("Niveau", col = lign2, numButton = 6, offset = 3)


  }


  var x:Int = 0
  var y:Int = 0

  override def onGraphicRender(g: GdxGraphics): Unit = {

    g.clear()

    g.setBackgroundColor(Color.valueOf("48d055"))

    if (haveMove) {
      val width: Int = g.getScreenHeight / (grid.grid.length + 2)
      x += action.actionX * width / 5
      y += action.actionY * width / 5

      haveMove = !grid.displayMove(g, oldGrid, action, x, y, nbMove, width)

    }
    else {
      grid.displayWin(g)
    }
    if (!haveMove) {

      x = 0
      y = 0
    }
    g.drawFPS()
    g.drawSchoolLogo()

  }

}
