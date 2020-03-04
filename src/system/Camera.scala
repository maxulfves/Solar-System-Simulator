package system


import geometry._

import java.awt.image.BufferedImage
import java.awt.{Graphics2D,Color,Font,BasicStroke}
import java.awt.geom._
import java.awt.Shape

//import o1._

class Camera(val plane:Plane, val focalPoint:Point) {
  
  //val focalPoint:Vector = new Vector(0,0,0)
  //val plane:Plane = new Plane(new Vector(0,0,-1), new Vector(100,0,0))
  
  /** Zooms in by moving the focalpoint away from the plane
   * 
   */
  def zoomIn() = {
    val f = plane.distanceTo(focalPoint)
    focalPoint.z += 0.01
    println(f)
    
  }
  
  /** Zooms out by moving the focalpoint closer to the plane in the direction of the normal vector. //TODO
   */
  
  def zoomOut() = {
    val f = plane.distanceTo(focalPoint)
    if(f > 0.02 ){
      focalPoint.z -= 0.01
      println(f )
    }else{
      println("%f > 0.0".format( f))
    }
  }
  
  
  val size = (1000, 1000)
  
  val const = 5.0e2
   
  var can2 = new BufferedImage(1000, 1000, BufferedImage.TYPE_INT_ARGB)
  
  
  def capture(system:System):BufferedImage = {
    //var canvas = rectangle(size._1, size._2, Black)
    
    val g2d:Graphics2D = can2.createGraphics();
    g2d.setColor(Color.BLACK)
    g2d.fillRect(0, 0, size._1, size._2)
    
    
    for(body <- system.bodies) {
      
      //Find the projected center point & velocity direction
      val line =       ( (body.location - focalPoint.posVector) ).toLine(focalPoint)
      val vector_loc = ( body.location  - focalPoint.posVector + (body.velocity * geometry.Constants.dt)) .toLine(focalPoint)
      val vector_acc = ( body.location  - focalPoint.posVector + (body.acceleration * math.pow(geometry.Constants.dt, 2)   )) .toLine(focalPoint)
      println(body.acceleration.magnitude)
      
      val o_location:Point = plane.intersects( line )
      val v_loc:Point = plane.intersects( vector_loc )
      val a_loc:Point = plane.intersects( vector_acc )
      
      
      //Find the distance between the center point and the projected point of ((body.location + plane.vector).unit * body.radius)
      val rad_vect = ((plane.normal.crossP(new Vector(0, 1, 0))).unit * body.getRadius )
      val radLoc = body.location + rad_vect
      
      
      val line_r = new Line( focalPoint, radLoc )
      val r_location:Point = plane.intersects( line_r )
      
      
      
      val rad2:Double = (o_location.posVector - r_location.posVector).magnitude 
      /*
      if(body.getName == "sun"){
        println(rad2)
      }*/
      
      //Sets the projected x and y values 
      
      var x = (o_location.x )
      var y = (o_location.y )
      

      
      //Sets the projected x and y values of the acceleration vector
      val accVect:(Int, Int) = ((size._1/2 + a_loc.x).toInt, (size._2/2 + a_loc.y).toInt)
      
      //Sets the projected x and y values of the velocity vector      
      val velVect:(Int, Int) = ((size._1/2 + v_loc.x).toInt, (size._2/2 + v_loc.y).toInt )
      
      
      //Draw what's seen onto the canvas. 
      val radius = 3
      val centerX = size._1/2 + x
      val centerY = size._2/2 + y
      
      g2d.setColor(Color.WHITE)
      g2d.drawLine(centerX.toInt, centerY.toInt, velVect._1, velVect._2)
      g2d.setColor(Color.RED)
      g2d.drawLine(centerX.toInt, centerY.toInt, accVect._1, accVect._2)
      
      
      val shape:Shape = new Ellipse2D.Double(centerX - radius, centerY - radius, 2.0 * radius, 2.0 * radius);
//        circle(10.0 , body.getColor)
      
      g2d.setColor(body.getColor)
      g2d.fill(shape)
      
      //val center = new Pos(size._1/2 + x, size._2/2 + y)
      
      /*
      //Draw velocity vector
      val vect_shape: Pic = Pic.line(new Pos(x, y), new Pos(x_v, y_v) , White)
      canvas = canvas.place(vect_shape, center)
      
      //Draw acceleration vector
      val acc_shape: Pic = Pic.line(new Pos(x, y), new Pos(x_a, y_a) , Red)
      canvas = canvas.place(acc_shape, center)
      */
      
      //TESTING
      /*
      if(body.getName == "earth") {
        println(body.getName + ": " + center.x + "/" + center.y)
        println("Line: " + line)
        println("o_location: " + o_location)
        println("X/Y: " + x + "/" + y)
        println("Center.x/center.Y: " + center.x + "/" + center.y)
        
      }*/
      
    }
    //println("----------------------------------------")
    
    can2
    
  }
  
  
  

}

