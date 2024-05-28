package classes

class Cellule (var isObstacl: Boolean = false, var haveSquatter: Boolean = false, private var valueInt: Int = 0)  {
  def getValueInt:Int = valueInt
  def setValueInt(value:Int):Unit = {
    valueInt = value
    haveSquatter = true
  }
}
