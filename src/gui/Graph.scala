package gui
import scala.swing._
import java.awt.Color
import scala.collection.mutable.Buffer
import java.awt.BasicStroke
import java.math.MathContext

class Graph extends Component{
  //preferredSize = new Dimension( 300, 300 )
  private val data:Buffer[(Double, Double)] = Buffer()
  private var maxX = Double.MinValue
  private var maxY = Double.MinValue
  private var minX = Double.MaxValue
  private var minY = Double.MaxValue
  
  
  object graphData {
    var bodyA: Option[system.Body] = None
    var bodyB: Option[system.Body] = None
    var field: Option[String] = None
    var fieldB: Option[String] = None
    var xTime: Boolean = true
  }
  
  def length = data.length
  
  def addData(x:Double, y:Double) {
  	val a = (x,y)
  	if(a._1 < minX){
  		minX = a._1
  	}
  	
  	if(a._1 > maxX){
  		maxX = a._1
  	}
  	if(a._2 < minY){
  		minY = a._2
  	}
  	if(a._2 > maxY){
  		maxY = a._2
  	}
  	
  	data += a
  }
  
  def clear{
  	data.clear()
 		maxX = Double.MinValue
		maxY = Double.MinValue
		minX = Double.MaxValue
		minY = Double.MaxValue
  }
  
  def scale(a:(Double, Double)): (Int, Int) = {
  	val x = ( (a._1 - minX )/(maxX - minX) ) * size.width
  	val y = size.height - ( ((a._2-minY)/(maxY-minY)) * size.height )
  	
  	return ( x.toInt, y.toInt )
  	
  }
  
  
  
  private val rounder = new MathContext(2)
  def roundDouble(d:Double):String = {
    var ret = d
    if(d.isInfinite() || d.isNaN()){
		  ret = 0
	  }

    val bd = BigDecimal(ret)
    val x = bd.round(rounder)
    x.toString()
  }
  
  
	override def paint(g2:Graphics2D) = {
		
		g2.setColor(Color.decode("#eeeeee"))
		g2.fillRect(0, 0, size.width, size.height)
		g2.setFont(font)
		g2.setColor(Color.decode("#999999"))
		for(i <- 0 to 9){
		  val delta = (maxY - minY).abs / 10
		  val h = size.height - ((size.height / 10) * i) -1
		  
		  /*
		  if(delta.isInfinite()){
		    delta = 0
		  }*/
		  var num:Double = minY + delta * i
		  //val d = BigDecimal(num)
		  //val x = d.round(rounder)
		  
		  
		  g2.drawLine(0, h, size.width, h)
		  g2.drawString(roundDouble(num), 2, h - 1)
		}
		
		val myFont = this.font
    val affineTransform = g2.getTransform
    affineTransform.rotate(Math.toRadians(90), 0, 0);
    val rotatedFont = myFont.deriveFont(affineTransform);
    g2.setFont(rotatedFont);

    for(i <- (0 to 9).reverse){
      val h =  ((size.width / 10) * i) -1
      g2.drawLine(h, 0, h, size.height)
      g2.drawString( roundDouble((minX + (maxX - minX)/10*i)), h + 2, 3)
    }
    
		
		g2.setColor(Color.decode("#222222"))
				
		for(pair <- data) {
			
			val sc = scale(pair)
			g2.fillRoundRect(sc._1, sc._2, 5, 5, 3, 3)
		}
		
		
    g2.dispose();
		
		repaint()
		revalidate()
		
	}
}