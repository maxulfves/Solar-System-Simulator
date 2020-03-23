package system


import geometry.Vector
import java.awt.Color

sealed abstract class Body(name:String, var mass:Double, radius:Double, val location:Vector, val velocity:Vector, system:System) {
  
  
  var nextloc = new Vector(0,0,0)
  
  def updatePos(dt:Double):Unit = {
    
    location += (velocity * dt)
    
  }
  
  
  /** Changes the body's location and velocity in the system using Runge-Kutta's 4th order method. 
   * @param Δt Delta time
   */
  
  var i = 0

  
  def rk2(Δt: Double):Unit = {
    
    val k1 = netAccelerationAt(location) * Δt
    val k2 = netAccelerationAt(location + k1) * Δt
    acceleration = (k1 + k1) * 0.5
    
    if(getName == "earth" ){
      //println(i + ", " +  ( (k1*Δt).magnitude - acc.magnitude) )
    }
        
    velocity += acceleration
    location += (velocity * Δt)
    
    i += 1
    
  }
  
  def rk3(Δt: Double):Unit = {
    
    val k1 = netAccelerationAt(location)      * Δt
    val k2 = netAccelerationAt(location + (k1*0.5))      * Δt
    val k3 = netAccelerationAt(location + (k2*(3.0/4) )) * Δt
    
    acceleration = (k1 + (k2 * 4) + k3) * (1.0/6.0)
    
    if(getName == "earth" ){
      //println(i + ", " +  ( (k1*Δt).magnitude - acc.magnitude) )
    }
    
    velocity += acceleration
    location += (velocity * Δt)
    
    i += 1
    
  }
  
    def rk4(Δt: Double):Unit = {
      
      val k1 = netAccelerationAt(location) * Δt
      val k2 = netAccelerationAt(location + (k1*0.5 )) * Δt
      val k3 = netAccelerationAt(location + (k2*0.5)) * Δt
      val k4 = netAccelerationAt(location + (k3)) * Δt
      
      acceleration =  (
          (k1) + 
          (k2 * 2 ) + 
          (k3 * 2 ) + 
          (k4)
        ) / 6.0 
      velocity += acceleration
      location += (velocity * Δt)
    
    
  }
  
  var acceleration:Vector = new Vector(0,0,0)
    
    
  def leapFrog(Δt: Double):Unit = {
    
    velocity.set(velocity + netAccelerationAt(location) * Δt / 2)
    location.set(location + velocity * Δt)
    velocity.set(velocity + netAccelerationAt(location) * Δt / 2)

  }
  
  
  
  
  def getMass = mass
  def getRadius = radius
  def getVelocity = velocity
  def getLocation = location
  def getName = name
  
  
  def setMass(m:Double) = {
    mass = m
  }
  

    /** Find the net acceleration recieved by a system at a given location. 
     * @param location The location in the system as a position vector. 
     * @return The net acceleration vector. 
     */
    
    def netAccelerationAt(location:Vector):Vector = {
      val acc = new Vector(0,0,0)
      
      //Iterate through every body in the system and determine its acceleration-component
      for(other <- system.bodies.filter(_ != this)) {
        
        val distance:Vector = other.location - location
        
        // (UNIT V) * G * OTHER.MASS / DISTANCE.magn ^2 
        val dA:Vector = (distance.unit * -1)  * (-geometry.Constants.G) * other.getMass / math.pow(distance.magnitude, 2)
        
        
        acc += dA
        
        
      }
      
      
      acc
  }
  
  def copy(system:System):Body
  
  private var color = Color.WHITE
  def setColor(other:Color) {
    color = other
  }
  def getColor(): Color = color

  
  
  
}


/** A planet in a solar system. 
 *
 *  @constructor create a new planet with a name, mass, radius, location and velocity 
 *  @param name name of the planet. 
 *  @param radius the planet's radius in meters. 
 *  @param location a position vector describing the planet's position
 *  @param velociy a vector describing the velocity of the planet
 *  @param system the system in which the planet exists
 */
class Planet(name:String, mass:Double, radius:Double, loc:Vector, velocity:Vector, system:System) extends Body(name, mass, radius, loc, velocity, system){
  override def toString = "Planet " + name+ " at " + location.toString() + " moving at " + velocity.toString()
    
  override def copy(system:System):Planet = {
    val ret = new Planet(this.name, this.mass, this.radius, this.loc.copy, this.velocity.copy, system)
    ret.setColor(getColor) 
    ret
  }
}

class Star(name:String, mass:Double, radius:Double, loc:Vector, velocity:Vector, system:System) extends Body(name, mass, radius, loc, velocity, system){
  override def toString = "Star " + name+ " at " + location.toString() + " moving at " + velocity.toString()
  def copy(system:System):Star = {
    val ret = new Star(this.name, this.mass, this.radius, this.loc.copy, this.velocity.copy, system)
    ret.setColor(getColor)
    ret
  }
}

class Satelite(name:String, mass:Double, radius:Double, loc:Vector, velocity:Vector, system:System, icon: Int) extends Body(name, mass, radius, loc, velocity, system){
  override def toString = "Satelite " + name+ " at " + location.toString() + " moving at " + velocity.toString()
  def copy(system:System):Satelite = new Satelite(this.name, this.mass, this.radius, this.loc.copy, this.velocity.copy, system, icon)
  final object Icon { 
    val ARROW = 0
    val CROSS = 1
    val CIRCLE = 2
  }

}

