package geometry

/**
 * A mathematical line as defined by the equation <br>
 * <b><i>f(t) = point + t * vector</i></b>
 * 
 * @author Max Ulfves
 */
class Line(val point:Point, var vector:Vector) {
  val unit = vector.unit
  /*
   * Line formula:
   * x = x0 + t * (xi)
   * y = y0 + t * (xj)
   * z = z0 + t * (xk)
   */
    
  
  /**
   * Find the point at t:Double
   * @return A point.
   */
  
  def at(t:Double):Point = {
    val ret = point + (vector * t )
    ret
  }
  
  override def toString() = {
    "Line: x = (%f + (t * %f)), y = (%f + (t * %f)), z = (%f + (t * %f))".format(point.x, vector.x, point.y, vector.y, point.z, vector.z )
  }
  
}