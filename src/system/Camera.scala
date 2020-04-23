package system

import geometry._

import java.awt.image.BufferedImage
import java.awt.{ Graphics2D, Color, Font, BasicStroke }
import java.awt.geom._
import java.awt.Shape

/**
 * A camera that captures an image representation of a system. The camera is always aimed at a fixed point, in this case (0,0,0) and can be rotated around it to view it from multiple angles.
 * 	@param plane The image plane.
 * 	@param focalPoint Focalpoint that is placed behind the plane.
 *  @param vectorUp What direction should be viewed as up on an image?
 */
class Camera() {

  private val d: Double = 3e12 //m
  private val f: Double = 2.0 //m

  private val plane = new Plane(0, 0, 1, d)
  def getDistance = plane.getD

  //var focalPoint: Point = new geometry.Point(0, 0, -(d + f))
  val vectorUp: Vector = new geometry.Vector(0, 1, 0)

  private val radiusScale: Double = 2 << 3
  private val radiusConstant: Double = 2.0 //px
  private val focalRange = (1.0, 10000.0) //in meter

  //private val closestPointOnPlane = plane.closestPointTo(focalPoint)

  private val origo2D: Point = plane.closestPointTo(new Point(0,0,0))

  private var renderVectors = true
  
  def toggleVectots {
    renderVectors = !renderVectors
  }
  
  private var focalLength = (focalRange._1 + focalRange._2) / 2

  def getFocallength = focalLength
  private def fPoint = origo2D + (plane.normal * focalLength)

  //These vectors describe the direction and magnitude of a pixel.
  private val yVect = vectorUp.unit
  private val xVect = yVect.crossP(plane.normal).unit

  private object original {
    val _origo2D: Point = new Point(origo2D.x, origo2D.y, origo2D.z)
    val _yVect: Vector = yVect.copy
    val _xVect: Vector = xVect.copy
    val dis = plane.getD
  }

  private var rotationX = 0.0
  private var rotationY = 0.0
  private var rotationZ = 0.0
  def getRotX = rotationX
  def getRotY = rotationY
  def getRotZ = rotationZ

  /** Sets the rotation to a particular value*/
  def rotateTo(angleX: Double, angleY: Double, angleZ: Double) = {
    rotationX = (angleX) % (math.Pi * 2)
    rotationY = (angleY) % (math.Pi * 2)
    rotationZ = (angleZ) % (math.Pi * 2)
    updateRotation
  }

  /** Alters the rotation by a particular value */
  def rotateBy(angleX: Double, angleY: Double, angleZ: Double) = {
    rotationX += angleX
    rotationY += angleY 
    rotationZ += angleZ 

    updateRotation
  }

  /** Sets the focallength of the camera. Note that the values aren't realworld values.  */
  def setFocalLength(flen: Double): Unit = {
    focalLength = flen
    updateRotation
  }

  private var distance = original.dis
  /** Sets the distance from the camera to the point. */
  def setDistance(dis: Double): Unit = {
    origo2D.set((plane.normal * dis).toPoint)

    distance = dis
    setFocalLength(focalLength)

  }

  /** Recalculates all values. Should be called after the state of the object is altered.  */
  private def updateRotation {
    var rm = Matrices.identity(3)
      .crossP(Matrices.rotationZ(rotationZ))
      .crossP(Matrices.rotationX(rotationX))
      .crossP(Matrices.rotationY(rotationY))

    //Rotate origo
    origo2D.set(original._origo2D.posVector.unit.*(distance).crossP(rm).toPoint())

    //Redefine plane:
    val normal = origo2D.posVector.unit * -1
    val d = -((normal.x * origo2D.x) + (normal.y * origo2D.y) + (normal.z * origo2D.z))
    

    xVect.set(original._xVect.crossP(rm).unit)
    yVect.set(original._yVect.crossP(rm).unit)

    plane.set(normal.x, normal.y, normal.z, d)

  }

  /**
   * Zooms in by moving the focalpoint away from the plane
   */
  def zoomIn = {
    val newFocalLength: Double = focalLength * 1.01

    if (newFocalLength < focalRange._2) {
      focalLength *= 1.01
      //focalPoint = fPoint
    } else {
      focalLength = focalRange._2
      //focalPoint = fPoint
    }
  }

  /**
   * Zooms out by moving the focalpoint closer to the plane in the direction of the normal vector. //TODO
   */
  def zoomOut = {
    val newFocalLength: Double = focalLength * 0.99

    if (newFocalLength > focalRange._1) {
      focalLength *= 0.99
      //focalPoint = fPoint
    } else {
      focalLength = focalRange._1
      //focalPoint = fPoint
    }
  }
  /** Increases camera's distance to center*/
  def moveOut = this.setDistance(distance * 1.1)

  /** Increases camera's distance to center*/
  def moveIn = if (distance > 1000) this.setDistance(distance * 0.9)

  private def componentFactors(point: Point): (Double, Double) = {
    //Distance between projected origin and point
    val vect = new Vector(origo2D.x - point.x, origo2D.y - point.y, origo2D.z - point.z)

    val A = new Matrix(Array(
      Array(xVect.x, yVect.x, plane.normal.x, vect.x),
      Array(xVect.y, yVect.y, plane.normal.y, vect.y),
      Array(xVect.z, yVect.z, plane.normal.z, vect.z)))
    
    A.gaussJordan
    
    //val ret = A.gj2(Array(vect.x, vect.y, vect.z))

    (A.getRow(0)(A.width-1), A.getRow(1)(A.width-1))

  }

  /**
   * Helper method that calculates the location of a point in space on the image plane.
   */
  private def projectOnCanvas(p: Vector): (Double, Double) = {
    val line: Line = ((p - fPoint.posVector)).toLine(p.toPoint)
    val o_loc: Point = plane.intersects(line)
    val centerProjection = componentFactors(o_loc) // (x, y)

    return centerProjection
  }

  /**
   * Captures an image of the system.
   * @param system The system that should be captured
   * @param width The width of the camera's image plane
   * @param height The height of the camera's image plane
   * @return An image of the system as seen by the camera
   */
  def capture(system: System, width: Int, height: Int): BufferedImage = {
    val canvas = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g2d: Graphics2D = canvas.createGraphics();
    g2d.setColor(Color.BLACK)
    g2d.fillRect(0, 0, width, height)

    val sorted = system.bodies.sortBy(n => (n.location - this.fPoint.posVector).magnitude).reverse

    for (body <- sorted) {

      ///FINDING THE RADIUS
      //Find a unit vector perpendicular to the plane and multiply it by the radius

      val r = (body.location - fPoint.posVector).crossP(yVect).unit
      val rad_vect = r * body.getRadius

      //A origin vector representing a point on the radius. Goal is to find the projected distance of this point to the center
      val radLoc = body.location + rad_vect

      //Plane's intersection with a line through the aforementioned point and the focal point.
      val lin = (radLoc - fPoint.posVector).toLine(fPoint)
      val r_location: Point = plane.intersects(lin)

      /////////////////
      //Find the projected center of each point. NOTE: Orgin assumed to be (0, 0), not center of screen.

      val centerProjection = projectOnCanvas(body.location)
      val vLoc = projectOnCanvas(body.location + (body.velocity * 3600*24*7)) 
      val aLoc = projectOnCanvas(body.location + (body.acceleration  * 3600*24*7 * 1e6)) 

      val rLoc = componentFactors(r_location)

      //Finally calculate the projected length of the radius.
      val egRad = math.sqrt(math.pow(centerProjection._1 - rLoc._1, 2) + math.pow(centerProjection._2 - rLoc._2, 2))
      var radius: Double = radiusConstant + math.pow(egRad, 0.5) * radiusScale

      //Sets the projected x and y values of the velocity vector arrow.
      val velVect: (Int, Int) = ((width / 2 + vLoc._1).toInt, (height / 2 + vLoc._2).toInt)

      //Sets the projected x and y values of the acceleration vector
      val accVect: (Int, Int) = ((width / 2 + aLoc._1).toInt, (height / 2 + aLoc._2).toInt)

      val centerX = width / 2 + centerProjection._1
      val centerY = height / 2 + centerProjection._2

      //Shape of the body.
      val shape: Shape = new Ellipse2D.Double(
        centerX - radius,
        centerY - radius,
        2.0 * radius,
        2.0 * radius);

      //Don't draw objects behind image plane.
      if ((fPoint - plane.normal.unit).distanceTo(body.location.toPoint()) > (fPoint + plane.normal.unit).distanceTo(body.location.toPoint())) {

        if (renderVectors) {
          
          g2d.setColor(Color.WHITE)

          g2d.drawLine(centerX.toShort, centerY.toShort, velVect._1, velVect._2)

          g2d.setColor(Color.RED)
          g2d.drawLine(centerX.toInt, centerY.toInt, accVect._1, accVect._2)

        }

        g2d.setColor(body.getColor)
        g2d.fill(shape)
      }
    }

    canvas

  }

}

