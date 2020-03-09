package geometry


/**
 * A mathematical plane as defined by the equation <br>
 * <i>Ax + By + Cz + D = 0</i>
 * 
 * @param A,B,C,D
 * @author Max Ulfves
 */
class Plane(var A:Double, var B:Double, var C:Double, var D:Double) {
  //Plane equation: Ax + By + Cz + D = 0
  
  
  def set( _A:Double, _B:Double,  _C:Double, _D:Double) = {
    this.A = _A
    this.B = _B
    this.C = _C
    this.D = _D
    
  }
  
  def normal:Vector = new Vector(A, B, C)
  
  /**
   * Find the point where the plane intersects with the given line.
   */
  
  def distanceTo(point: Point):Double = {
    val d:Double = math.abs(A * point.x + B * point.y + C * point.z + D) / 
                   normal.magnitude
     
    d
  }
  
  
  
  def closestPointTo(point:Point):Point = this.intersects(new Line(point, normal)) 
  
  
  def intersects(line: Line): Point = {
    //Lägg linjens x,y,z värden in i P och lös ut t. 
    val t:Double = 
      -(A * line.point.x + B * line.point.y + C * line.point.z + D ) /
      ( A * line.vector.x + B * line.vector.y + C * line.vector.z )
      
      //val t:Double = ( (A * line.xp + B * line.yp + C * line.zp + D) / (A * (line.xp - line.xq) + B * (line.yp-line.yq) + C * (line.zp-line.zq)) ) 
    //The line crosses the plane at f(t) = line.at(t)
    line.at(t)
  }
  
  override def toString = "Plane: (%f * x + %f * y + %f * z + %f = 0)".format(A, B, C, D)
  
  
  
}