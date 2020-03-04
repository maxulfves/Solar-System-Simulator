package geometry

/**
 * A mathematical line as defined by the equation <br>
 * <b><i>f(t) = point + t * vector</i></b>
 * 
 * @author Max Ulfves
 */
class Line(val point:Point, val vector:Vector) {
  val unit = vector.unit
  
  /*
   * Line formula:
   * x = xp + t * (xq - xp)
   * y = yp + t * (yq - yp)
   * z = zp + t * (zq - zp)
   */
    
  
  /**
   * Find the point at t:Double
   * @return A point.
   */
  def at(t:Double):Point = {
    point + (vector*t)
  }
  
  override def toString() = {
    "Line: x = (%f + (t * %f)), y = (%f + (t * %f)), z = (%f + (t * %f))".format(point.x, vector.x, point.y, vector.y, point.z, vector.z )
    
  }
  
}