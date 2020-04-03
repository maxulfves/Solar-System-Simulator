package system

import scala.collection.mutable._
import geometry._
import java.util.Calendar

/**
 * This class represents a solar system. It has a collection of bodies.
 *
 * @autohr Max Ulfves
 * @version 0.1
 * @since 12.02.2020
 */
class System( var name: String ) {
	val bodies = Buffer[Body]()

	val date = Calendar.getInstance()
	val months = Array( "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER" )
	date.setTimeInMillis( 0 )

	def getTimeInMs = date.getTimeInMillis
	def getDate: String = date.get( Calendar.YEAR ) + " " + months( date.get( Calendar.MONTH ) ).substring( 0, 3 ) + " " + "%02d".format( date.get( Calendar.DAY_OF_MONTH ) )

	/**
	 * The position vector of the center point of the solar system.
	 * @return physics.Vector Returns the center off mass.
	 */
	def centerOfMass: Vector = {
		val center: geometry.Vector = new geometry.Vector( 0, 0, 0 )

		for ( body <- bodies ) {
			center += ( body.location * body.getMass )
		}

		center / totalMass
	}

	def remove( body: Body ): Unit = {
		bodies -= body
	}

	/**
	 * Returns the total mass of the solar system.
	 */
	def totalMass: Double = {
		var massSum: Double = 0.0
		for ( body <- bodies ) {
			massSum += body.getMass
		}
		massSum
	}

	def addBody( body: Body ) = {
		bodies += body
	}

	def getName = this.name
	def setName( other: String ) = {
		name = other
	}
	def setTime( other: Long ) = {
		date.setTimeInMillis( other )
	}

	def copy: System = {
		//TODO Fyll ut

		val ret = new System( getName )
		//ret.bodies++=this.bodies

		for ( body <- this.bodies ) {
			ret.bodies += body.copy( ret )
		}

		ret.date.set( this.date.get( Calendar.YEAR ), this.date.get( Calendar.MONTH ), this.date.get( Calendar.DAY_OF_MONTH ) )

		ret
	}

	def set( other: System ) = {
		//TODO Fyll ut
		this.bodies.clear()

		println( "resetted system" )

		for ( body <- other.bodies ) {
			addBody( body.copy( this ) )
		}

		this.date.set( other.date.get( Calendar.YEAR ), other.date.get( Calendar.MONTH ), other.date.get( Calendar.DAY_OF_MONTH ) )
		time = other.time
	}

	var time = 0.0

	/**
	 * Passes a certain ammount of time in the solar system.
	 *
	 */
	def update: Unit = {
		if ( time < geometry.Constants.dt * 365.25 || true ) {

			for ( body <- bodies ) {

				if ( !body.getIsGhost ) {
					body.rk4( geometry.Constants.dt )
				}

			}

			time += geometry.Constants.dt

			date.add( Calendar.SECOND, geometry.Constants.dt.toInt )

			//println(getDate)

		}
	}

}