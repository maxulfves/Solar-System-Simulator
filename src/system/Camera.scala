package system

import geometry._

import java.awt.image.BufferedImage
import java.awt.{ Graphics2D, Color, Font, BasicStroke }
import java.awt.geom._
import java.awt.Shape
import javax.imageio.ImageIO
import java.io.File

/**
 * A camera that captures an image representation of a system. The camera is always aimed at a fixed point, in this case (0,0,0) and can be rotated around it to view it from multiple angles.
 * 	@param plane The image plane.
 * 	@param focalPoint Focalpoint that is placed behind the plane.
 *  @param vectorUp What direction should be viewed as up on an image?
 */
class Camera() {

  private val d: Double = 3e12 //m
  private val f: Double = 2.0 //m

  def getDistance = distance

  private val radiusScale: Double = 2
  private val radiusConstant: Double = 2.0 //px
  private val radiusFeather: Double = 0.4

  private val focalRange = (3000, 7000.0) //in meter

  //The point in space that will be in the center of the screen.
  private val origo2D: Vector = new Vector(0, 0, d)

  private var renderVectors = true
  private var renderStars = true

  def toggleStars {
    renderStars = !renderStars
  }

  def toggleVectots {
    renderVectors = !renderVectors
  }

  private var focalLength = (focalRange._1 + focalRange._2) / 2
  def getFocalLength = focalLength

  private def fPoint = origo2D + (origo2D.unit * focalLength)

  private object original {
    val _origo2D: Vector = origo2D.copy
    val dis = origo2D.z
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
    rotationX = (rotationX + angleX) % (Math.PI * 2)
    rotationY = (rotationY + angleY) % (Math.PI * 2)
    rotationZ = (rotationZ + angleZ) % (Math.PI * 2)
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
    origo2D.set((origo2D.unit * dis))
    distance = dis
    setFocalLength(focalLength)
  }

  /** Recalculates all values. Should be called after the state of the object is altered.  */
  private def updateRotation {
    var rm = Matrices.identity(4)
      .crossP(Matrices.rotationZ(rotationZ))
      .crossP(Matrices.rotationX(rotationX))
      .crossP(Matrices.rotationY(rotationY))
    //Rotate origo
    origo2D.set(original._origo2D.unit.*(distance).crossP(rm))
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

  /** Zooms out by moving the focalpoint closer to the plane in the direction of the normal vector. //TODO*/
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

  /** Method that calculates the location of a point in space on the image plane.*/
  private def projectOnCanvas(p: Vector): (Double, Double) = {
    val r = p.crossP(toCameraSpace).crossP(perspectiveMatrix())
    return ((r.x / distance) * focalLength, (r.y / distance) * focalLength)
  }

  private def toCameraSpace: Matrix = {
    Matrices.rotation(-rotationX, -rotationY, -rotationZ)
      .crossP(Matrices.translate(-origo2D.x, -origo2D.y, -origo2D.z))

  }

  //A rewritten version of opengl's perspective function.
  private def perspectiveMatrix(): Matrix = {
    val ratio = 3f / 3f
    val angle = math.Pi / 6
    val near = 1e3
    val far = 1e30

    val ret = new Matrix(Array(
      Array(1.0 / (ratio * angle), 0, 0, 0),
      Array(0, 1.0 / angle, 0, 0),
      Array(0, 0, -(far + near) / (far - near), -1),
      Array(0, 0, -(2 * far * near) / (far - near), 0)))
    ret
  }

  //For rendering the background.
  private class Star(val vector: Vector, val size: Double)

  private val stars = Vector.fill(1000)(new Star(
    new Vector(-1 + 2 * math.random(), -1 + 2 * math.random(), -1 + 2 * math.random()).unit * (1),
    1 + 6 * math.random()))

  /**
   * Captures an image of the system.
   * @param system The system that should be captured
   * @param width The width of the camera's image plane
   * @param height The height of the camera's image plane
   * @return An image of the system as seen by the camera
   */
  def capture(system: System, width: Int, height: Int): BufferedImage = {
    
    val canvas = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g2d: Graphics2D = canvas.createGraphics();
    g2d.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON);
    g2d.setRenderingHint(java.awt.RenderingHints.KEY_DITHERING, java.awt.RenderingHints.VALUE_DITHER_ENABLE);

    //Set black background
    g2d.setColor(Color.BLACK)
    g2d.fillRect(0, 0, width, height)

    //Draw stars
    if (renderStars) {
      for (star <- stars) {

        if (star.vector.crossP(toCameraSpace).z > 0) {
          val centerProjection = projectOnCanvas((star.vector * (distance / 4)))
          val center = (width / 2 + centerProjection._1, height / 2 + centerProjection._2)

          val starShape: Shape = new Ellipse2D.Double(
            center._1,
            center._2,
            star.size,
            star.size)

          g2d.setColor(Color.WHITE)
          g2d.fill(starShape)
        }
      }
    }
    //Sort the bodies by their Z-value.
    val sorted = system.bodies.sortBy(n => n.location.crossP(toCameraSpace).z).reverse
    for (body <- sorted) {

      val centerProjection = projectOnCanvas(body.location)

      //Finally calculate the projected length of the radius.
      val radius: Double = radiusConstant + radiusScale * math.pow(body.getRadius * (1.0 / math.tan(math.Pi / 6 / 2)) / ((origo2D + body.location) / 1e5).magnitude, radiusFeather) * 4

      val center = (width / 2 + centerProjection._1, height / 2 + centerProjection._2)
      //Shape of the body.
      val shape: Shape = new Ellipse2D.Double(
        center._1 - radius,
        center._2 - radius,
        2.0 * radius,
        2.0 * radius);

      //Draw the body first

      g2d.setColor(body.getColor)
      g2d.fill(shape)

      //A boolean for toggling vectors on and off
      if (renderVectors) {
        //I've made the choice by experimenting that the values below works best in representing the vectors.
        val vLoc = projectOnCanvas(body.location + (body.velocity * 3600 * 24 * 7))
        val aLoc = projectOnCanvas(body.location + (body.acceleration * 3600 * 24 * 7 * 1e6))

        //Sets the projected x and y values of the velocity vector arrow.
        val velVect: (Int, Int) = ((width / 2 + vLoc._1).toInt, (height / 2 + vLoc._2).toInt)
        //Sets the projected x and y values of the acceleration vector
        val accVect: (Int, Int) = ((width / 2 + aLoc._1).toInt, (height / 2 + aLoc._2).toInt)

        //Then draw the vectors.
        g2d.setColor(Color.WHITE)
        g2d.drawLine(center._1.toShort, center._2.toShort, velVect._1, velVect._2)
        g2d.setColor(Color.RED)
        g2d.drawLine(center._1.toInt, center._2.toInt, accVect._1, accVect._2)
      }
    }
    canvas
  }
}

