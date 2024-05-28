package classes

abstract class Direction {
  var actionX:Int
  var actionY:Int
}
class North() extends Direction{
  override var actionX: Int = 0
  override var actionY: Int = -1
}
class South() extends Direction{
  override var actionX: Int = 0
  override var actionY: Int = 1
}
class East() extends Direction{
  override var actionX: Int = 1
  override var actionY: Int = 0
}
class West() extends Direction{
  override var actionX: Int = -1
  override var actionY: Int = 0
}