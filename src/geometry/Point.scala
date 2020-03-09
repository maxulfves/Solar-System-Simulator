package geometry

class Point(var x:Double, var y:Double, var z:Double) {
  def bob = 100
  
  def +(vector:Vector):Point = new Point(getX + vector.x, y + vector.y, z + vector.z)
  def -(vector:Vector):Point = new Point(getX - vector.x, y - vector.y, z - vector.z)
  
  override def toString() = "(%f, %f, %f)".format(x, y, z)
  
  def posVector = new Vector(x, y, z)
  
  def +=(v:Vector) {
    this.x += v.x
    this.y += v.y
    this.z += v.z
  }
  
  def -=(v:Vector) {
    this.x -= v.x
    this.y -= v.y
    this.z -= v.z
  }
  
  def getX = x
  def getY = y
  def getZ = z
  
  def set(other:Point) {
    this.x = other.getX
    this.y = other.getY
    this.z = other.getZ
  }
  
}