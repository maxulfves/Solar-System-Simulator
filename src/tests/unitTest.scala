package tests


import org.junit.Test
import org.junit.Assert._

import geometry._
import java.awt.Color

import geometry._
import system._

/*
 * Unit tests for polynomials.
 *
 */

class UnitTests2 {

  @Test def vectorTests() {
    
    val vect = new Vector(1,2,3)
    val expected = 3.7416573867739 // +- 1e-12
    
    //Length
    assert(math.abs(vect.magnitude - 3.7416573867739) < 1e-12, ("The length of a vector (1,2,3) is wrong. Should be  ", expected, ", was ", vect.magnitude) )
    
    //vect.unit
    assert(vect.unit.magnitude == 1.00, ("Length of unit vector was not 1. It was: "+  vect.unit.magnitude))
   
    //vect.+=()
    vect.+=(new Vector(4,4,4))
    assert( vect.x == 5 && vect.y == 6 && vect.z == 7, "Operation += returned a dubious value.")
    
    //vect.-=()
    vect-=new Vector(4,4,4)
    assert( vect.x == 1 && vect.y == 2 && vect.z == 3, "Operation -= returned a dubious value.")
    
    //crossP
    val vectA = new Vector(4,5,6)
    val vectB = new Vector(7,6,5)
    val crossP = vectA.crossP(vectB)
    assert( crossP.x == -11 && crossP.y == 22 && crossP.z == -11)
    
    
  }
  
  @Test def planeTests(){
    val plane = new Plane(0, 0, 1, 10)
    val origo = new Point(0,0,0)
    
    //Distance to origo
    val distance = plane.distanceTo(origo)
    assert(distance == 10, "Expected distance to be 10.0, was: " + distance)
    
    //Distance to point on plane
    val pointOnPlane = new Point(0 , 0, -10)
    val secondDistance = plane.distanceTo(pointOnPlane)
    assert(secondDistance == 0, "Expected distance to be 0.0, was " + secondDistance)
    
    
    
  }
  
  @Test def lineTest(){
    val line = new Line(new Point(10,5,5), new Vector(1, 1, 1))
    val plane = new Plane(0,0,1,0)
    
    println("point :" + plane.intersects(line))
    
    assert(plane.intersects(line).posVector.magnitude < 1e-4 )
    
    //assert(line.at(10))
    
  }
  
  
  
  @Test def cameraTest(){
/*
    val system = new System("test", 10000, 20)
    val planet = new Planet("testPlanet", 22, 1, new Vector(0,5500,0), new Vector(0,0,0), system)
    system.addBody(planet)
    
    val plane = new Plane(0,0,1, -10)
    val fp = new Point(0,0,11)    
    val up = new Vector(1,0,0)
    val camera = new Camera(plane, fp, up)
    
    //Test that body is visible
    val capt = camera.capture(system, 1000, 1000)
    val black = Color.BLACK
    val color = new Color(capt.getRGB(500, 500));
    
    
    //Test plane geometry
    val line = new Line(fp, planet.location - fp.posVector) //( (planet.location - fp.posVector) ).toLine(fp)
    val o_location:Point = plane.intersects( line )
    */
  }


  
  
}


