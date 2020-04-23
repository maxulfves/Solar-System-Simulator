package tests

import org.junit.Test
import geometry._

class MatrixTest {
/*
  @Test def gaussJordan() {
    val A = new Matrix(Seq(
          Seq(1, 0, 0, 5),
          Seq(0, 1, 0, 6),
          Seq(0, 0, 1, 0)
    ))
    
    assert(A.gaussJordan(0) == 5, "A.gaussJordan(0) was " + A.gaussJordan(0))
    assert(A.gaussJordan(1) == 6, "A.gaussJordan(1) was " + A.gaussJordan(1))
    assert(A.gaussJordan(2) == 0, "A.gaussJordan(2) was " + A.gaussJordan(2))
    
    
  }
  */
  @Test def gaussJordanWithZeroRow() {
    
    val A = new Matrix(Array(
          Array(2, 1, 0, 1),
          Array(0, 1, 0, -1),
          Array(0, 0, 1, 1)
    ))
    
    A.gaussJordan
    
    println(A)
    
    assert(A.getRow(0)(0) == 1)
    assert(A.getRow(1)(1) == 1)
    assert(A.getRow(2)(2) == 1)
    
    assert(A.getRow(0)(3) == 1)
    assert(A.getRow(1)(3) == -1)
    
  }
  
  @Test def rotateX(){
    val vect = new Vector(0,0,1)
    val mat = Matrices.rotationX(math.Pi / 2)
    
    val test = vect.crossP(mat)
    val correct = new Vector(0, -1, 0)
    
    val errorMargin = (test - correct).magnitude
    assert(errorMargin < 1e-15, "Rotate X failed. Error was: " + errorMargin)
  }
  
}