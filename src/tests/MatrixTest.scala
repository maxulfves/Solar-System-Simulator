package tests

import org.junit.Test
import geometry._

class MatrixTest {

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
  
  @Test def translate(){
    val A = new Vector(1.0,2.0,3.0)
    val B = Matrices.translate(1.0, 2.0, 3.0)
    val C = A.crossP(B)
    val exp = new Vector(2,4,6)
   
    assert((C - exp).magnitude <  1e-15, "(1,2,3) translated by (1,2,3) should give "+ exp +". Gave : " + C)
  }
  
  @Test def VectorMagnitude(){
    val a = new Vector(1,5,10)
    val exp = 11.224972160
    assert(a.magnitude - exp < 1e-5)
  }
  
  @Test def VectorUnit(){
    val A = new Vector(20, 0, 0)
    val B = A.unit
    val exp = new Vector(1,0,0)
   
    assert((B - exp).magnitude < 1e-5, "Difference shouldn be zero. Was: "+ (B - exp))
  }  
  
}