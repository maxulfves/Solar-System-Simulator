

package geometry

import java.text.DecimalFormat

/**
 * A mathematical vector.
 *
 *  @constructor create a new vector with x-, y- and z-values.
 *  @param x X-val
 *  @param y Y-val
 *  @param z Z-val
 */
class Vector(
    private var x_ : Double, 
    private var y_ : Double, 
    private var z_ : Double) {

  override def toString(): String = {

    val rY = Math.round(y_ * 100.0) / 100.0
    val rZ = Math.round(z_ * 100.0) / 100.0
    "(" + x_ + " | " + y_ + " | " + z_ + ")"
  }
  
  
  def x = this.x_
  def y = this.y_
  def z = this.z_
  
  
  def copy = new Vector(x, y, z)

  private def roundDouble(d: Double): String = {
    val formatter: DecimalFormat = new DecimalFormat();
    formatter.setMaximumFractionDigits(3)
    formatter.format(d)
  }

  /**
   * Cross product of two vectors
   * @param other The vector with which <u>this</u> should be multiplied.
   * @returns A new vector
   */
  def crossP(other: Vector): Vector = new Vector(
    this.y_ * other.z_ - this.z_ * other.y_,
    this.z_ * other.x_ - this.x_ * other.z_,
    this.x_ * other.y_ - this.y_ * other.x_)

  /**
   * Cross product of vector and matrix
   * @param other The vector with which <u>this</u> should be multiplied.
   * @return A new Vector
   */
  
  def toMatrix:Matrix = new Matrix(Array(Array(x),Array(y),Array(z), Array(1)) )
  
  def crossP(matrix: Matrix): Vector = {

    val ret = this.toMatrix.crossP(matrix)
    new Vector(ret(0,0), ret(0,1), ret(0,2))
  }

  /** Dot product of two vectors. */
  def dotP(other: Vector): Double = {
    this.x_ * other.x_ + this.y_ * other.y_ + this.z_ * other.z_
  }
  

  /**
   * Angle between this vector and another vector.
   * @param other Another vector
   * @return Angle given in radians
   */
  def angle(other: Vector): Double = {
    math.acos(this.dotP(other) / (this.magnitude * other.magnitude))

  }


  /**
   * Sets the vector-components to those of the vector given as a parameter
   * @param other Another Vector-object
   */
  def set(other: Vector): Unit = {
    this.x_ = other.x_
    this.y_ = other.y_
    this.z_ = other.z_
  }

  /** Magnitude (length) of the Vector-object. */
  def magnitude: Double = {
    math.sqrt(x_ * x_ + y_ * y_ + z_ * z_)
  }
  
  /** Increases the length of the Vector object by the length of the parameter-vector*/
  def +=(other: Vector): Unit = {
    x_ = x_ + other.x_
    y_ = y_ + other.y_
    z_ = z_ + other.z_
  }

  /** Subtracts the parameter vector from this vector-object*/
  def -=(other: Vector): Unit = {
    x_ = x_ - other.x_
    y_ = y_ - other.y_
    z_ = z_ - other.z_
  }
  
  /**
   * Division by a Double
   * @return A new Vector
   */
  def /(value: Double): Vector = {
    new Vector(x_ / value, y_ / value, z_ / value)
  }
  
  /**
   * The unit-vector of this vector. 
   * @return A new vector.
   */
  def unit: Vector = {
    val m = magnitude
    if(m > 0){
      this / magnitude
    }else{
      this
    }
  }
  
  /** Vector multiplication*/
  def *(other: Double): Vector = {
    new Vector((x * other.doubleValue()), (y * other.doubleValue()), (z * other.doubleValue()))
  }
  

  /*
  def *(other: Vector): Vector = {
    new Vector(x * other.x, y * other.x, z * other.x)
  }*/
  
  /** Vector addition*/
  def +(other: Vector): Vector = {
    new Vector(x_ + other.x_, y_ + other.y_, z_ + other.z_)
  }

  /** Vector subtraction*/
  def -(other: Vector): Vector = {
    new Vector(x_ - other.x_, y_ - other.y_, z_ - other.z_)
  }

}