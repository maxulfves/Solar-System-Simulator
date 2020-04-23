package system

import scala.collection.mutable._
import geometry._
import java.util.Calendar
import scala.swing.Dialog

/**
 * This class represents a solar system. It has a collection of bodies.
 *
 * @autohr Max Ulfves
 * @version 0.1
 * @since 12.02.2020
 */
class System(private var name: String, private var timeStep:Long, private var endTime:Long) {
  
  val bodies = Buffer[Body]()
  
  private var done = false
  def getDone = done
  def setDone() = {done = true}
  
  val date = Calendar.getInstance()
  val months = Array("JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")
  date.setTimeInMillis(0)
  
  def getTimeInMs = date.getTimeInMillis
  def getDate: String = date.get(Calendar.YEAR) + " " + months(date.get(Calendar.MONTH)).substring(0, 3) + " " + "%02d".format(date.get(Calendar.DAY_OF_MONTH))

  /**
   * The position vector of the center point of the solar system.
   * @return physics.Vector Returns the center off mass.
   */
  def centerOfMass: Vector = {
    val center: geometry.Vector = new geometry.Vector(0, 0, 0)

    for (body <- bodies) {
      center += (body.location * body.getMass)
    }

    center / totalMass
  }

  def remove(body: Body): Unit = {
    bodies -= body
  }

  /** Returns the total mass of the solar system.*/
  def totalMass: Double = {
    var massSum: Double = 0.0
    for (body <- bodies) {
      massSum += body.getMass
    }
    massSum
  }

  def addBody(body: Body) = {
    bodies += body
  }

  def getName = this.name
  def setName(other: String) = {
    name = other
  }
  def setTime(other: Long) = {
    date.setTimeInMillis(other)
  }

  def getEnd = endTime
  def getStepSize = timeStep
  def getTime = systemTime

  def copy: System = {
    //TODO Fyll ut

    val ret = new System(name, this.getEnd, this.getStepSize)
    //ret.bodies++=this.bodies

    for (body <- this.bodies) {
      ret.bodies += body.copy(ret)
    }
    
    ret.date.set(this.date.get(Calendar.YEAR), this.date.get(Calendar.MONTH), this.date.get(Calendar.DAY_OF_MONTH))

    ret
  }

  def set(other: System) = {
    //TODO Fyll ut
    this.bodies.clear()

    println("resetted system")

    for (body <- other.bodies) {
      addBody(body.copy(this))
    }

    this.date.set(other.date.get(Calendar.YEAR), other.date.get(Calendar.MONTH), other.date.get(Calendar.DAY_OF_MONTH))
    systemTime = other.systemTime
  }
  
  private var systemTime = 0.0

  /** Passes a certain ammount of time in the solar system.*/
  def update: Unit = {
    if (!done && systemTime < endTime ) {
      
      //Updates all velocities
      for (body <- bodies) {
        body.passTime(timeStep)
      }

      //Checks if body hits any other body.
      for(body <- bodies){
        val path = new Line(body.location.toPoint(), body.velocity)
        for(other <-bodies - body){
          if((other.location - body.location).magnitude < body.getRadius + other.getRadius + (body.velocity*timeStep).magnitude){
            val vect = (body.location - other.location).crossP(body.velocity)
            val d = vect.magnitude / body.velocity.magnitude - other.getRadius - body.getRadius
             
            if(d < 0){
              Dialog.showMessage(null, "Reason: "+body.getName + " collided with " + other.getName + "!" , title="Simulation complete!")
              setDone()
              return
            }else{
              println(d)
            } 
          } 
        }
      }
      if(!getDone){
        for (body <- bodies) {
          //Moves all bodies
    			body.updatePosVel(timeStep)
        }
      }
      systemTime += timeStep

      date.add(Calendar.SECOND, timeStep.toInt)


    }else if(!done && systemTime >= endTime){
      Dialog.showMessage(null, "Reason: Timeframe ended" , title="Simulation complete!")
      setDone()
    }
  }

  def setEnd(arg: Long) = {
    this.endTime = arg
    
  }
  def setStepSize(arg:Long) {
    this.timeStep = arg
  }

}