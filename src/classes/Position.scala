package classes

class Position(var x : Int,var y : Int) extends Serializable {

  override def toString () : String = {
  return s"x : $x, y : $y"

  }

}

