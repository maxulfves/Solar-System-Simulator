package geometry

/**
 * A mathematical plane as defined by the equation <br>
 * <i>Ax + By + Cz + D = 0</i>
 *
 * @param A,B,C,D
 * @author Max Ulfves
 */
class Plane(private var A: Double, private var B: Double, private var C: Double, private var D: Double) {

  /**
   * Sets the plane parameters.
   */
  def set(_A: Double, _B: Double, _C: Double, _D: Double) = {
    this.A = _A
    this.B = _B
    this.C = _C
    this.D = _D

  }

  def getD = this.D

  /** Returns a normal-vector to the plane.*/
  def normal: Vector = new Vector(A, B, C)

  /** Finds the distance to a given point. */
  def distanceTo(point: Point): Double = {
    val d: Double = math.abs(A * point.x + B * point.y + C * point.z + D) /
      normal.magnitude

    d
  }
  
  /** Finds the closest point to the plane. */
  def closestPointTo(point: Point): Point = this.intersects(new Line(point, normal))

  /** Finds the intersection point with a given line.*/
  def intersects(line: Line): Point = {
    val t: Double =
      -((A * line.point.x) + (B * line.point.y) + (C * line.point.z) + D) /
        ((A * line.vector.x ) + (B * line.vector.y ) + (C * line.vector.z ))
        
    line.at(t)
  }

  override def toString = "Plane: (%f * x + %f * y + %f * z + %f = 0)".format(A, B, C, D)

}