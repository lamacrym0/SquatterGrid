package classes

abstract class Direction {
  var actionX:Int
  var actionY:Int
}

case class North() extends Direction{
  override var actionX: Int = 0
  override var actionY: Int = -1
}

case class South() extends Direction{
  override var actionX: Int = 0
  override var actionY: Int = 1
}

case class East() extends Direction{
  override var actionX: Int = 1
  override var actionY: Int = 0
}

case class West() extends Direction{
  override var actionX: Int = -1
  override var actionY: Int = 0
}