

package geometry

import java.text.DecimalFormat

/** A mathematical vector.
 *
 *  @constructor create a new vector with x-, y- and z-values.
 *  @param x X-val
 *  @param y Y-val
 *  @param z Z-val
 */
class Vector(var x:Double, var y:Double, var z:Double) {
  
  override 
  def toString():String = {
    
    val rY = Math.round(y*100.0)/100.0
    val rZ = Math.round(z*100.0)/100.0
    "(" + roundDouble(x) + "/" + roundDouble(y) + "/" + roundDouble(z) + ")"
  }
  
  def copy = new Vector(x,y,z)
  
  private def roundDouble(d:Double):String = {
    val formatter:DecimalFormat = new DecimalFormat();
    formatter.setMaximumFractionDigits(3)
    formatter.format(d)
  }
  
  def crossP(other:Vector):Vector = new Vector(
      this.y * other.z - this.z * other.y,
      this.z * other.x - this.x * other.z,
      this.x * other.y - this.y * other.x)
  
  def crossP(matrix:Matrix):Vector = {
    var ret:Array[Double] = Array(0, 0, 0)
    
    for( i <- 0 until matrix.height){
      ret(i) += matrix.seq(i)(0) * this.x
      ret(i) += matrix.seq(i)(1) * this.y
      ret(i) += matrix.seq(i)(2) * this.z
    }
    new Vector(ret(0), ret(1), ret(2))
  }
   
  
  def dotP(other:Vector):Double = {
      this.x * other.x + this.y * other.y + this.z * other.z
  }
  
  /** Angle between this vector and another vector.
   *  @param other Another vector
   * @returns Angle given in radians
   */
  def angle(other:Vector):Double = {
    math.acos( this.dotP(other)/(this.magnitude * other.magnitude) )
    
  }
  
  def toPoint():Point = new Point(x,y,z)
  
  def toLine(point: Point):Line = {
    new Line(point, this)
  }
  
  def set(other: Vector):Unit = {
    this.x = other.x
    this.y = other.y
    this.z = other.z
  }
  
  def magnitude:Double = {
    math.sqrt(x*x + y*y + z*z)
  }
  
  def +=(other:Vector):Unit = {
    x = x + other.x
    y = y + other.y
    z = z + other.z
  }
  
  def -=(other:Vector):Unit = {
    x = x - other.x
    y = y - other.y
    z = z - other.z
  }
  
  def /(value:Double):Vector = {
    /*x = x/value
    y = y/value
    z = z/value*/
    new Vector(x/value,y/value,z/value)
  }
  
  def unit:Vector = {
    this/magnitude
  }
  /*
  def *(other:Vector):Vector = {
    ???
  }*/
  
  
  def *(other:Number):Vector = {
    new Vector(x*other.doubleValue(), y*other.doubleValue(), z*other.doubleValue())
  }
  
  
  
  def *(other:Vector):Vector = {
    new Vector(x*other.x, y*other.x, z*other.x)
  }
  
  def +(other:Vector):Vector = {
    new Vector(x+other.x, y+other.y, z+other.z)
  }
  
  def -(other:Vector):Vector = {
    new Vector(x-other.x, y-other.y, z-other.z)
  }
  
}