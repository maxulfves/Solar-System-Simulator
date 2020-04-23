package geometry

/**
 * A helper object for the Matrix-class
 * Stores rotation matrices.
 *
 */
object Matrices {

  def rotationX(angle: Double): Matrix = {
    new Matrix(Array(
      Array(1, 0, 0),
      Array(0, math.cos(angle), -math.sin(angle)),
      Array(0, math.sin(angle), math.cos(angle))))
  }

  def rotationZ(angle: Double): Matrix = new Matrix(Array(
    Array(math.cos(angle), -math.sin(angle), 0.0),
    Array(math.sin(angle), math.cos(angle), 0.0),
    Array(0.0, 0.0, 1.0)))

  def rotationY(angle: Double): Matrix = new Matrix(Array(
    Array(math.cos(angle), 0.0, math.sin(angle)),
    Array(0, 1, 0),
    Array(-math.sin(angle), 0.0, math.cos(angle))))

  def zeroMatrix(size: Int): Matrix = new Matrix(Array.fill(size)(Array.fill(size)(0)))

  def identity(size: Int): Matrix = {
    val mat = zeroMatrix(size)
    for (i <- 0 until mat.height by 1) {
      mat.set(i, i, 1)
    }

    mat
  }

}