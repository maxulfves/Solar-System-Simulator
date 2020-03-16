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
  
  
  //def focalLength = plane.distanceTo(focalPoint)
  //focalLength = 0.050
  
  def focalLength = plane.distanceTo(focalPoint)
  
  
  val radiusScale:Double = 2 << 3
  val radiusConstant:Double = 2.0 //px
  val imgRes:(Double, Double) = (1000, 1000 ) //px
  val focalRange = (0.1, 80) //in meter
  
  ///TEST
  val stars:Array[Point] = Array.tabulate(1000)( _ => makeStar(1e20))
  
  
  def makeStar(radius:Double):Point = {
    val theta = math.random() * math.Pi * 2.0
    val x = math.random() * 2 - 1.0
    val y = math.cos(theta) * math.sqrt(1 - (x * x))
    val z = math.sin(theta) * math.sqrt(1 - (x * x))
    //println(z + ", " +  y)
    new Point(x * radius, y * radius ,z * radius)
  }
  
  //END TEST
  
  private val closestPointOnPlane = plane.closestPointTo(focalPoint)
  
  
  
  val origo2D:Point = plane.closestPointTo(focalPoint)
  
  
  var fLen2 = focalLength
  def fPoint = origo2D + plane.normal * fLen2
  
  
  //These vectors describe the direction and magnitude of a pixel.
  val yVect = vectorUp.unit 
  val xVect = yVect.crossP(plane.normal).unit 
  
  
  val planePoints:Seq[Point] = Seq(
    origo2D + ((xVect * imgRes._1) + (yVect * imgRes._2))  / 2,
    origo2D + ((xVect * imgRes._1) - (yVect * imgRes._2))  / 2,
    origo2D - ((xVect * imgRes._1) + (yVect * imgRes._2))  / 2,
    origo2D - ((xVect * imgRes._1) - (yVect * imgRes._2))  / 2
  )
  
  
  object original {
    val _origo2D:Point = new Point(origo2D.x, origo2D.y, origo2D.z)
    val _yVect:Vector = yVect.copy
    val _xVect:Vector = xVect.copy
  }
  
  
  var rotationX = 0.0
  var rotationY = 0.0
  var rotationZ = 0.0
  
  def rotateTo(angleX:Double, angleY:Double, angleZ:Double) = {
    rotationX = ( angleX) % (math.Pi * 2)
    rotationY = ( angleY) % (math.Pi * 2)
    rotationZ = ( angleZ) % (math.Pi * 2)
    updateRotation
  }
  
  def rotateBy(angleX:Double, angleY:Double, angleZ:Double) = {
    rotationX = (rotationX + angleX) % (math.Pi * 2)
    rotationY = (rotationY + angleY) % (math.Pi * 2)
    rotationZ = (rotationZ + angleZ) % (math.Pi * 2)
    
    updateRotation
  }
  
  def setFocalLength(flen:Double) = {
    fLen2 = flen
    updateRotation
  }
  
  private def updateRotation{
    var rm = Matrices.identity(3)
      .crossP(Matrices.rotationY(rotationY))
      .crossP(Matrices.rotationX(rotationX))
      .crossP(Matrices.rotationZ(rotationZ))
    
    
    //Rotate three arbitrary points on the image plane. 
    /*for(point <- planePoints){
      point.set(point.posVector.crossP(rm).toPoint)
    }*/
    
    //Rotate origo
    origo2D.set(original._origo2D.posVector.crossP(rm).toPoint())
    
    
    //Redefine plane: 
    val normal = origo2D.posVector.unit * -1
    val d = - ((normal.x * origo2D.x) + (normal.y * origo2D.y) + (normal.z * origo2D.z) )
    
    //Rotate focalpoint
    focalPoint.set(origo2D - (normal * fLen2) )
    
    
    xVect.set(original._xVect.crossP(rm).unit )
    yVect.set(original._yVect.crossP(rm).unit )
    
    
    plane.set(normal.x, normal.y, normal.z, d)
    t += 1
    
    
  }
  
  private val zoomStep = 0.1
  /** Zooms in by moving the focalpoint away from the plane
   */
  def zoomIn = {
    val newFocalLength:Double = fLen2 + zoomStep
    
    if(newFocalLength < focalRange._2){
      focalPoint -= (plane.normal.unit * zoomStep)
      fLen2 += zoomStep
    }else{
      focalPoint.set(closestPointOnPlane - (plane.normal.unit * focalRange._2) )
      fLen2 = focalRange._2
    } 
  }   
  
  
  /** Zooms out by moving the focalpoint closer to the plane in the direction of the normal vector. //TODO
   */
  def zoomOut = {
    val newFocalLength:Double = focalLength - zoomStep
    
    if(newFocalLength > focalRange._1 ){
      focalPoint += (plane.normal.unit * zoomStep)
      fLen2 -= zoomStep
    }else{
      focalPoint.set(closestPointOnPlane - (plane.normal.unit * focalRange._1) )
      fLen2 = focalRange._1
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
  

  
   
  var can2 = new BufferedImage(imgRes._1.toInt, imgRes._2.toInt, BufferedImage.TYPE_INT_ARGB)
  
  
  var t = 0
  def capture(system:System):BufferedImage = {
    
    val g2d:Graphics2D = can2.createGraphics();
    g2d.setColor(Color.BLACK)
    g2d.fillRect(0, 0, imgRes._1.toInt, imgRes._2.toInt)
    
    val sorted = system.bodies.sortBy( n =>  (n.location - this.focalPoint.posVector).magnitude).reverse
    
    /*
     * ATTEMPT TO ADD STARS TO GIVE THE VIEWER A REFERENCE POINT. 
    val stars_lines:Array[Line] = stars.map( star => ( (star.posVector - focalPoint.posVector) ).toLine(focalPoint) ) 
    val starIntersection:Array[Point] = stars_lines.map(star => plane.intersects(star))
    val stars2D:Array[(Double, Double)] = starIntersection.map(star => componentFactors(star))
      .map( b =>  ( b._1 + imgRes._1, b._2 + imgRes._2) )
    val rad = 10
    for(star <- stars2D){
      g2d.setColor(Color.WHITE)
      g2d.fillOval( (star._1-rad).toInt , (star._2-rad).toInt , (2*rad).toInt, (2*rad).toInt );
    }*/
      
    for(body <- sorted) {
      //Find the projected center point & velocity direction
      val vector_pos:Line = ( (body.location - focalPoint.posVector) ).toLine(body.location.toPoint)
      val vector_loc:Line = ( body.location  - focalPoint.posVector + (body.velocity * geometry.Constants.dt)) .toLine(focalPoint)
      val vector_acc:Line = ( body.location  - focalPoint.posVector + (body.acceleration * math.pow(geometry.Constants.dt, 2) * 0.2   )) .toLine(focalPoint)
    
      
      //Find the intersection with camera plane. 
      val o_loc:Point = plane.intersects( vector_pos )
      val v_loc:Point = plane.intersects( vector_loc )
      val a_loc:Point = plane.intersects( vector_acc )
      
      ///FINDING THE RADIUS
       //Find a unit vector perpendicular to the plane and multiply it by the radius 
      
      val r = (body.location - focalPoint.posVector).crossP(yVect).unit
      val rad_vect = r * body.getRadius
      
      //A origin vector representing a point on the radius. Goal is to find the projected distance of this point to the center 
      val radLoc = body.location + rad_vect
      
      
      //Plane's intersection with a line through the aforementioned point and the focal point. 
      val lin = (radLoc-focalPoint.posVector).toLine(focalPoint)
      val r_location:Point = plane.intersects( lin )
      
      /////////////////
      //Find the projected center of each point. NOTE: Orgin assumed to be (0, 0), not center of screen.
      
      val centerProjection = componentFactors(o_loc) // (x, y)
      val vLoc = componentFactors(v_loc)
      val aLoc = componentFactors(a_loc)
      val rLoc = componentFactors(r_location)
        
      
      //Finally calculate the projected length of the radius.
      val egRad = math.sqrt( math.pow(centerProjection._1 - rLoc._1, 2) + math.pow(centerProjection._2 - rLoc._2, 2) )
      var radius:Double = radiusConstant + math.pow(egRad, 0.5)  * radiusScale
      
      if(body.getName == "earth"){
        t+=1
        
        /*
        println("plane	" + plane.A + " | " + plane.B + " | " + plane.C + " | " + plane.D)
        
        println("Bloc:	" + body.location)
        println("Oloc:	" + o_loc)
        
        println("IS:	  " + plane.intersects(vector_pos))
        println("LineV:	" + vector_pos.vector)
        println("LineP:	" + vector_pos.point)
        
        println("Cent:  " + centerProjection._1 + " | " + centerProjection._2)
        println("---------")*/
      }
      
      //Sets the projected x and y values of the velocity vector arrow.  
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
      
      
      //Draw what the camera sees. 
      //TODO Don't draw objects outside the camera's field of view.
      if( (focalPoint - plane.normal.unit).distanceTo(body.location.toPoint()) > (focalPoint + plane.normal.unit).distanceTo(body.location.toPoint()) ){
        g2d.setColor(Color.WHITE)
        g2d.drawLine(centerX.toInt, centerY.toInt, velVect._1, velVect._2)
        g2d.setColor(Color.RED)
        //g2d.drawLine(centerX.toInt, centerY.toInt, accVect._1, accVect._2)
        
        g2d.setColor(body.getColor)
        g2d.fill(shape)
      }else{
        //println(body.getName)
      }
      


      
    }
    
    can2
    
  }
  
  
  

}

