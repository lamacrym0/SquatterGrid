package classes

class Cellule (var isObstacl: Boolean = false, var haveSquatter: Boolean = false, private var valueInt: Int = 0) extends Serializable  {

  def getValueInt:Int = valueInt

  def setValueInt(value:Int):Unit = {
    valueInt = value

    if(value != 0) haveSquatter = true
    else haveSquatter = false
  }
}
