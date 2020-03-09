package system


import geometry._

import java.awt.image.BufferedImage
import java.awt.{Graphics2D,Color,Font,BasicStroke}
import java.awt.geom._
import java.awt.Shape


/** A camera that captures a system.
 *	@param plane The image plane.
 * 	@param focalPoint Focalpoint that is placed behind the plane.
 *  @param vectorUp What direction should be viewed as up on an image?
 */
class Camera(val plane:Plane, var focalPoint:Point, val vectorUp:Vector) {
  
  
  def focalLength = plane.distanceTo(focalPoint)
  
  
  val radiusScale:Double = 2 << 10 
  val radiusConstant:Double = 1.0 //px
  val imgRes = (1000, 1000 ) //px
  val focalRange = (0.012, 0.100) //in meter
  
  
  private val closestPointOnPlane = plane.closestPointTo(focalPoint)
  

  val origo2D:Point = plane.closestPointTo(focalPoint)
  
  
  //These vectors describe the direction and magnitude of a pixel.
  val yVect = vectorUp.unit / (1000 * 270)
  val xVect = yVect.crossP(plane.normal).unit / (1000 * 270)
  
  
  val planePoints:Seq[Point] = Seq(
    origo2D + ((xVect * imgRes._1) + (yVect * imgRes._2))  / 2,
    origo2D + ((xVect * imgRes._1) - (yVect * imgRes._2))  / 2,
    origo2D - ((xVect * imgRes._1) + (yVect * imgRes._2))  / 2,
    origo2D - ((xVect * imgRes._1) - (yVect * imgRes._2))  / 2
  )
  

  var t = 0
  
  def rotate(angle:Double) = {
    val rm = Matrices.rotationX(angle )
    
    
    //Rotate three arbitrary points on the image plane. 
    /*for(point <- planePoints){
      point.set(point.posVector.crossP(rm).toPoint)
    }*/
    
    //Rotate origo
    origo2D.set(origo2D.posVector.crossP(rm).toPoint())
    
    
    //Redefine plane: 
    val normal = origo2D.posVector.unit * -1
    val d = (normal.x * origo2D.x) + (normal.y * origo2D.y) + (normal.z * origo2D.z)
    
    //Rotate focalpoint
    focalPoint.set(origo2D - (normal * focalLength) )
    
    xVect.set(xVect.crossP(rm).unit / (1000 * 270) ) 
    yVect.set(yVect.crossP(rm).unit / (1000 * 270) )
        
    
    plane.set(normal.x, normal.y, normal.z, -d)
    t += 1
    
    
  }
  
  
  /** Zooms in by moving the focalpoint away from the plane
   * 
   */
  def zoomIn = {
    val newFocalLength:Double = focalLength + zoomStep
    if(newFocalLength < focalRange._2){
      focalPoint -= (plane.normal.unit * zoomStep)
      //println("oFL: " + focalLength)
      
    }else{
      focalPoint.set(closestPointOnPlane - (plane.normal.unit * focalRange._2) )
      //println( "oFL: " + focalLength )
    }
    
  }
  
  
  private val zoomStep = 0.001
  /** Zooms out by moving the focalpoint closer to the plane in the direction of the normal vector. //TODO
   */
  def zoomOut = {
    val newFocalLength:Double = focalLength - zoomStep
    
    if(newFocalLength > focalRange._1 ){
      focalPoint += (plane.normal.unit * zoomStep)
      //println( "FL: " + focalLength )
    }else{
      focalPoint.set(closestPointOnPlane - (plane.normal.unit * focalRange._1) )
      //println( "FL: " + focalLength )
    }
    
  }
  
  private def componentFactors(point: Point):(Double, Double) = {
    //Distance between projected origin and point
    val vect = new Vector(origo2D.x - point.x, origo2D.y - point.y, origo2D.z - point.z)
    
    val A = new Matrix(Array(
        Array(xVect.x, yVect.x, plane.normal.x),
        Array(xVect.y, yVect.y, plane.normal.y),
        Array(xVect.z, yVect.z, plane.normal.z)
    ))
    
    
    
    val ret = A.gj2(Array(vect.x, vect.y, vect.z))
    
    
    
    (ret(0), ret(1))
    
    
    
  }
  

  
   
  var can2 = new BufferedImage(imgRes._1, imgRes._2, BufferedImage.TYPE_INT_ARGB)
  
  
  def capture(system:System):BufferedImage = {
    
    val g2d:Graphics2D = can2.createGraphics();
    g2d.setColor(Color.BLACK)
    g2d.fillRect(0, 0, imgRes._1, imgRes._2)
    
    val sorted = system.bodies.sortBy( n=>  (n.location - this.focalPoint.posVector).magnitude)
    for(body <- sorted) {
      
      //Find the projected center point & velocity direction
      val vector_pos:Line =       ( (body.location - focalPoint.posVector) ).toLine(focalPoint)
      val vector_loc:Line = ( body.location  - focalPoint.posVector + (body.velocity * geometry.Constants.dt)) .toLine(focalPoint)
      val vector_acc:Line = ( body.location  - focalPoint.posVector + (body.acceleration * math.pow(geometry.Constants.dt, 2) * 0.2   )) .toLine(focalPoint)
      
      //Find the intersection with camera plane. 
      val o_loc:Point = plane.intersects( vector_pos )
      val v_loc:Point = plane.intersects( vector_loc )
      val a_loc:Point = plane.intersects( vector_acc )
      
      
      ///FINDING THE RADIUS
       //Find a unit vector perpendicular to the plane and multiply it by the radius 
       //new Vector(plane.normal.z, plane.normal.x, plane.normal.y)
      val rad_vect = (plane.normal.crossP(  this.vectorUp  )).unit * body.getRadius 
      
      //A origin vector representing a point on the radius. Goal is to find the projected distance of this point to the center 
      val radLoc = body.location + rad_vect
      
      
      //Plane's intersection with a line through the aforementioned point and the focal point. 
      val lin = (radLoc-focalPoint.posVector).toLine(focalPoint)
      val r_location:Point = plane.intersects( lin )
      
      //Finally calculate the projected length of the radius.
      val egRad = (o_loc.posVector - (r_location.posVector)  ).magnitude
      val radiusScale = 2 << 14
      val radius:Double = radiusConstant + math.pow(egRad, 0.5)  * radiusScale
      
      
      
      /////////////////
      
      //Find the projected center of each point. NOTE: Orgin assumed to be (0, 0), not center of screen. 
      val centerProjection = componentFactors(o_loc) // (x, y)
      val vLoc = componentFactors(v_loc)
      val aLoc = componentFactors(a_loc)
      
      
      //Sets the projected x and y values of the velocity vector      
      val velVect:(Int, Int) = ((imgRes._1/2 + vLoc._1).toInt, (imgRes._2/2 + vLoc._2).toInt )
      
      
      //Sets the projected x and y values of the acceleration vector
      val accVect:(Int, Int) = ((imgRes._1/2 + aLoc._1).toInt, (imgRes._2/2 + aLoc._2).toInt)
      
      
      val centerX = imgRes._1/2 + centerProjection._1
      val centerY = imgRes._2/2 + centerProjection._2
      
      //Shape of the body.
      val shape:Shape = new Ellipse2D.Double(
        centerX - radius, 
        centerY - radius, 
        2.0 * radius, 
        2.0 * radius);
      
      if(body.getName == "earth"){
        println(t + "," + centerProjection._2)
        t += 1
      }
      
      
      //Draw what the camera sees. 
      //TODO Don't draw objects outside the camera's field of view. 
      
      g2d.setColor(Color.WHITE)
      g2d.drawLine(centerX.toInt, centerY.toInt, velVect._1, velVect._2)
      g2d.setColor(Color.RED)
      //g2d.drawLine(centerX.toInt, centerY.toInt, accVect._1, accVect._2)
      
      
      g2d.setColor(body.getColor)
      g2d.fill(shape)

      
    }
    
    can2
    
  }
  
  
  

}

