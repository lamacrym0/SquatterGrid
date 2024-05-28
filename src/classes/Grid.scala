package classes

import scala.collection.mutable.ArrayBuffer

object test extends App{
  var grid:Grid = new Grid(3,2)
  grid.display()
}
class Grid (x:Int,y:Int) {
  var grid:ArrayBuffer[ArrayBuffer[Cellule]] = ArrayBuffer()


  for( idy <- 0 until  y){
    grid.addOne(new ArrayBuffer[Cellule]())
    for(idx <- 0 until x){
      grid(idy).addOne(new Cellule())
    }
  }
  grid(1)(0).setValueInt(1)


  def display(): Unit = {
    for(y <- grid.indices ){
      for (x<- grid(y).indices ){
        if(!grid(y)(x).isObstacl){
          if(grid(y)(x).haveSquatter){
            print(grid(y)(x).getValueInt + " ")
          }else {
            print("0 ")
          }
        }
        else{
          print("X ")
        }
      }
      println()
    }
  }

  def gridIsFinish:Boolean = {
    for(y<-grid.indices){
      for(x<-grid(y).indices if(grid(y)(x).getValueInt == 0)){
        return false
      }
    }
    true
  }
  def move(action: Direction): Unit = {
    var headX: Int = getHeadPos(1)
    var headY: Int = getHeadPos(0)

    if(action.actionX + headX >= grid(0).length || action.actionX + headX < 0 || action.actionY + headY >= grid.length || action.actionY + headY < 0 ) {
      return
    }

    if(grid(headY + action.actionY)(headX + action.actionX).isObstacl || grid(headY + action.actionY)(headX + action.actionX).haveSquatter)
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

}


