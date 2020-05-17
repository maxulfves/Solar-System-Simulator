package system

import geometry.Vector
import java.awt.Color

class Body(var name: String, var mass: Double, private var radius: Double, val location: Vector, val velocity: Vector, system: System) {

  def setName(_name: String) {
    this.name = _name
  }

  def setLocation(loc: Vector) {
    location.set(loc)
  }

  def setVelocity(v: Vector) {
    velocity.set(v)
  }

  /** Updates all all velocities during delta t.  */
  def updatePosVel(Δt: Double) = {
    location.set(location + dxdt * Δt)
    velocity.set(velocity + acceleration * Δt)
    
  }
  
  private var dxdt = new Vector(0, 0, 0)
  val acceleration = new Vector(0, 0, 0)

  /** Solves the change in velocity (acceleration) during delta t. */
  def passTime(Δt: Double) = {
    class Derivative(val vel: Vector, val acc: Vector)
    class State(val pos: Vector, val vel: Vector) {} //Holder object for a position and velocity. 

    //Evaluates the value of the next state
    def findState(in: State, dt: Double, d: Derivative): Derivative = {
      val st = new State(
        in.pos + d.vel * dt,
        in.vel + d.acc * dt)

      val output: Derivative = new Derivative(
        st.vel, netAccelerationAt(st.pos))

      output
    }

    val state = new State(location, velocity)

    val k1 = findState(state, Δt, new Derivative(new Vector(0, 0, 0), new Vector(0, 0, 0)));
    val k2 = findState(state, Δt * 0.5f, k1);
    val k3 = findState(state, Δt * 0.5f, k2);
    val k4 = findState(state, Δt, k3);
    
    //Weighted average of the velocities and accelerations
    dxdt.set((k1.vel + (k2.vel + k3.vel) * 2.0 + k4.vel) / 6.0)
    acceleration.set((k1.acc + (k2.acc + k3.acc) * 2.0 + k4.acc) / 6.0)

  }

  def getMass = mass
  def getRadius = radius
  def setRadius(r: Double) = {
    this.radius = r
  }

  def getVelocity = velocity
  def getLocation = location
  def getName = name

  def setMass(m: Double) = {
    mass = m
  }

  /**
   * Find the net acceleration recieved by a system at a given location.
   * @param location The location in the system as a position vector.
   * @return The net acceleration vector.
   */
  def netAccelerationAt(location: Vector): Vector = {
    val acc = new Vector(0, 0, 0)

    //Iterate through every body in the system and determine its acceleration-component
    for (other <- system.bodies - this) {

      val distance: Vector = other.location - location

      // (UNIT V) * G * OTHER.MASS / DISTANCE.magn ^2
      val dA: Vector = (distance.unit * -1) * (-geometry.Constants.G) * other.getMass / math.pow(distance.magnitude, 2)

      if ((this.location - other.location).magnitude < this.radius + other.radius) {
        system.setDone()

      }

      acc += dA

    }

    acc
  }

  def copy(system: System): Body = {
    val ret = new Body(this.name, this.mass, this.radius, this.location.copy, this.velocity.copy, system)
    ret.setColor(getColor)
    ret
  }

  private var color = Color.WHITE
  def setColor(other: Color) {
    color = other
  }
  def getColor(): Color = color

  override def toString = "Planet " + name + " at " + location.toString() + " moving at " + velocity.toString()
}





