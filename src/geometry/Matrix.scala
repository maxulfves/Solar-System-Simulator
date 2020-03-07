package geometry
import scala.collection.mutable.Buffer

class Matrix( input: Seq[Seq[Double]] ) {
  require(input.forall(_.size == input(0).size ))
  
  val width = input(0).size      //m
  val height = input.size  //n
  
  
  var seq: Buffer[Buffer[Double]] = input.map( _.toBuffer ).toBuffer
  
  
  def this(n:Int, m:Int) = {
    this(Vector.fill(n)(Vector.fill(m)(0)))
  }
  
  def __==(other:Matrix) = {
  
  }
  
  /** Cross multiplies this Matrix with the argument.
   * A × B = C
   * @param B Another matrix. 
   * @returns C, new Matrix object
   */
  //Source: https://en.wikipedia.org/wiki/Matrix_multiplication_algorithm
  def crossP(B:Matrix):Matrix = {
    require(this.height == B.width, "Width height wrong")
    val m = this.height
    
    val C = new Matrix(width, B.height)
    
    for(i <- 0 to C.width -1 ){
      for(j <- 0 to C.height - 1){
        var sum:Double = 0
        for(k <- 0 to m - 1){
          sum = sum + (seq(i)(k) * B.seq(k)(j) )
        }
        C.seq(i)(j) = sum
      }
    }
    
    C
    
  }
  
  def determinand: Double = {
    require(this.width == this.height)
    val n = this.width 
    val lu = LU
    
    var detL:Double = 1.0
    var detU:Double = 1.0
    
    for(i <- 0 to n-1){
      detL = detL * lu._1.seq(i)(i)
      detU = detU * lu._2.seq(i)(i)
      
    }
    detL * detU
  }
  
  def LU:(Matrix, Matrix) = {
    require(width == height)
    
    var U = new Matrix(width, height) 
    var L = new Matrix(width, height) 
    
    for(i <- 0 to this.width-1){
      //Upper
      for(k <- i to this.width-1){
        var sum = 0.0
        for(j <- 0 to i){
          sum += (L.seq(i)(j)* U.seq(j)(k))
        }
        U.seq(i)(k) = seq(i)(k) - sum
      }
      
      //lower
      for(k <- i to this.width-1){
        if(i == k){
          L.seq(i)(i) = 1
        }else{
          var sum = 0.0 
          for(j <- 0 to i){
            sum = sum +  ( L.seq(k)(j) * U.seq(j)(i) ) 
          }
          L.seq(k)(i) = (seq(k)(i) - sum) / U.seq(i)(i)
        }
        
      }
      
    }
    
    
    
    (L, U)
  }
  
  
  def cofactor(x:Int, y:Int):Double = {
    new Matrix((seq.take(y) ++ seq.drop(y+1)).map(b => b.take(x) ++ b.drop(x+1))).determinand * math.pow(-1, x + y)
    
  }
  
  def adj:Matrix = {
    require(width == height)
    val ret = this.copy
    
    for( i <- 0 to height - 1 ){
      for(j <- 0 to width - 1){
        
        //val sign = if( (i+j)%2 == 0 ) 1 else -1
        
        ret.seq(i)(j) = cofactor(i,j)
        /*
        if(seq(i)(j) >= 0 && seq(i)(j) < width) {
          
        }
        */
      }
    }
    ret
  }
  
  def copy = new Matrix(seq.map(_.clone()).clone())
  
  def /(i:Double):Matrix = {
    val m = copy
    m.seq = seq.map(a => a.map(b => b/i))
    m
    
  }
  
  def inverse:Matrix = {
    require(this.width == this.height, "Can't inverse: W/H")
    require(determinand != 0, "Can't inverse: Det = 0")
    
    this.adj / determinand
    
  }
  
  override def toString = {
    seq.map( _.mkString(", ") ).mkString("\n") + "\n"
  }
  
  def gaussJordan:Seq[Double] = {
    require(width > 1, "Width too small")
    
    for(x <- 0 until width-1){
      //println(x)
    //Eliminate first column
    
    
    //make row  --> 1
    val n = seq(x)(x)
    
    for(w <- 0 until width){
      //println(seq(x)(w) + " / " + n)
      seq(x)(w) = seq(x)(w) / n
    }
    
    
    for(y <- x + 1 until height){
      val fact = seq(y)(x)
      //println(fact)
      
      
      //eliminate row
      //println("Y" + y)
      for(w <- 0 until width){
        //println("S " + seq(y)(w) +  "-" + (fact * seq(0)(w) ) ) 
        seq(y)(w) = seq(y)(w) - (fact * seq(x)(w))
        
        //println(this)
      }
    }
    }
    
    //Remove upper
    for(x <- 1 until width){
      for(y <- (x-1) to 0){
        val fact = seq(y)(x)
        
        for(w <- 0 until width){
          seq(y)(w) = seq(y)(w) - (fact * seq(x)(w))
        }
      }
    }
    
    
    //println(this)
    
    val ret:Buffer[Double] = Buffer()
    for(s <- this.seq){
      ret += s(this.width-1)
    }
    
    ret.toSeq
    
  }
  
  def dotP(B:Matrix):Matrix = {
    ???
  }
  
}