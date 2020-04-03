package gui
import scala.swing._
import java.awt.Color
import scala.collection.mutable.Buffer
import java.awt.BasicStroke

class Graph extends Component{
  preferredSize = new Dimension( 300, 100 )
  //val size = (300, 100)
  
  private val data:Buffer[(Double, Double)] = Buffer()
  private var maxX = Double.MinValue
  private var maxY = Double.MinValue
  private var minX = Double.MaxValue
  private var minY = Double.MaxValue
  
  def addData(x:Double, y:Double) {
  	val a = (x,y)
  	if(a._1 < minX){
  		minX = a._1
  		println("AX")
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
  
  val s = new BasicStroke(3f)
  
	override def paint(g2:Graphics2D) = {
		
		g2.setColor(Color.WHITE)
		g2.fillRect(0, 0, size.width, size.height)
		
		g2.setColor(Color.BLACK)
		g2.setStroke(s)
		
		for(pair <- data) {
			
			val sc = scale(pair)
			g2.drawLine(sc._1, sc._2, sc._1 + 1, sc._2 + 1)
		}
		
		repaint()
		revalidate()
		
	}
}