package geometry

object Matrices {
  
  def rotationX(angle:Double):Matrix = {
    new Matrix(Array(
      Array(1, 0, 0),    
      Array(0, math.cos(angle), -math.sin(angle) ),
      Array(0, math.sin(angle), math.cos(angle) )
    ))
  }
  
  def rotationZ(angle:Double):Matrix = new Matrix( Array(
    Array(math.cos(angle), - math.sin(angle), 0.0),
    Array(math.sin(angle), math.cos(angle), 0.0 ),
    Array(0.0, 0.0, 1.0)      
  ))
  
  
  def rotationY(angle:Double):Matrix = new Matrix( Array(
    Array(math.cos(angle), 0.0, math.sin(angle)),
    Array(0, 1, 0 ),
    Array(- math.sin(angle), 0.0, math.cos(angle) )      
  ))
  
  
  
}