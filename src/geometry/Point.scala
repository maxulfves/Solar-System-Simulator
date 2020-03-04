package geometry

class Point(var x:Double, var y:Double, var z:Double) {
  def bob = 100
  
  def +(vector:Vector):Point = new Point(x + vector.x, y + vector.y, z + vector.z)
  
  override def toString() = "(%f, %f, %f)".format(x, y, z)
  
  def posVector = new Vector(x, y, z)
  
}