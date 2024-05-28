package classes

import scala.collection.mutable.ArrayBuffer

object test extends App{
  var grid:Grid = new Grid(2,3)
  grid.display()
}
class Grid (x:Int,y:Int) {
  var grid:ArrayBuffer[ArrayBuffer[Cellule]] = ArrayBuffer()


  for( idx <- 0 until  x){
    grid.addOne(new ArrayBuffer[Cellule]())
    for(idy <- 0 until y){
      grid(idx).addOne(new Cellule())
    }
  }

  def display(): Unit = {
    for(x <- grid.indices ){
      for (y<- grid(x).indices ){
        if(grid(x)(y).isObstacl){
          if(grid(x)(y).haveSquatter){
            print("1 ")

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



}
