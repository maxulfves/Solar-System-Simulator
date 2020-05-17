package gui
import scala.swing._
import java.awt.Color
import scala.collection.mutable.Buffer
import java.awt.BasicStroke
import java.math.MathContext
import java.awt.image.BufferedImage

class Graph extends Component {
  //preferredSize = new Dimension( 300, 300 )
  private val data: Buffer[(Double, Double)] = Buffer()
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

  def addData(x: Double, y: Double) {
    val a = (x, y)
    if (a._1 < minX) {
      minX = a._1
    }

    if (a._1 > maxX) {
      maxX = a._1
    }
    if (a._2 < minY) {
      minY = a._2
    }
    if (a._2 > maxY) {
      maxY = a._2
    }

    data += a
  }

  def clear {
    data.clear()
    maxX = Double.MinValue
    maxY = Double.MinValue
    minX = Double.MaxValue
    minY = Double.MaxValue
  }

  def scale(a: (Double, Double)): (Int, Int) = {
    val x = ((a._1 - minX) / (maxX - minX)) * size.width
    val y = size.height - (((a._2 - minY) / (maxY - minY)) * size.height)

    return (x.toInt, y.toInt)

  }

  private var img: BufferedImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)

  //I decided to render the plotted graph on a separate thread to save processing power.
  //This way it wont increase the drawing time of simulationCanvasPanel as it did in an earlier implementation.
  private object graphThread extends Thread {
    private var oldDataLength = -1

    private val rounder = new MathContext(2)
    def roundDouble(d: Double): String = {
      var ret = d
      if (d.isInfinite() || d.isNaN()) {
        ret = 0
      }

      val bd = BigDecimal(ret)
      val x = bd.round(rounder)
      x.toString()
    }

    override def run() = {
      while (true) {
        oldDataLength += 1
        //Only updates if graph is visible and data set has been updated.
        if (size.width > 0 && size.height > 0 && oldDataLength != data.length) {

          val nImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB)
          val g2 = nImg.createGraphics()
          g2.setColor(Color.decode("#eeeeee"))
          g2.fillRect(0, 0, size.width, size.height)
          g2.setFont(font)
          g2.setColor(Color.decode("#999999"))
          for (i <- 0 to 9) {
            val delta = (maxY - minY).abs / 10
            val h = size.height - ((size.height / 10) * i) - 1

            var num: Double = minY + delta * i

            g2.drawLine(0, h, size.width, h)
            g2.drawString(roundDouble(num), 2, h - 1)
          }

          val affineTransform = g2.getTransform
          affineTransform.rotate(Math.toRadians(90), 0, 0);
          val rotatedFont = font.deriveFont(affineTransform);
          g2.setFont(rotatedFont);

          for (i <- (0 to 9).reverse) {
            val h = ((size.width / 10) * i) - 1
            g2.drawLine(h, 0, h, size.height)
            g2.drawString(roundDouble((minX + (maxX - minX) / 10 * i)), h + 2, 3)
          }

          g2.setColor(Color.decode("#222222"))

          for (pair <- data) {
            try{
              val sc = scale(pair)
              g2.fillRoundRect(sc._1, sc._2, 5, 5, 3, 3)
            }catch{
              
              case e:NullPointerException => println("Invalid input")
              
            }
          }
          
          img = nImg
          g2.dispose()

        }

        /* As the length of the data-buffer increases, this thread sleeps longer.
         * This is mainly to save resources. When there are ~60k data points the program starts to become slower.
         * Another solution would be do delete the earliest datapoints.
         */
        
        Thread.sleep(50 + (data.length / 50))
      }
    }
  }

  graphThread.start()

  override def paint(g: Graphics2D) = {

    g.drawImage(img, 0, 0, null)
    g.dispose();

    repaint()
    revalidate()

  }
}