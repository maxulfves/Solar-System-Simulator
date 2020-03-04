package system


import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon

// This program is introduced in Chapter 12.3 of the ebook.

import swing._
import event._
import geometry._
import java.awt.image.BufferedImage
import java.awt.Color


object TestApp extends SimpleSwingApplication  {
    import event.Key._
    import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle}
    import java.awt.{Color => AWTColor}

  
  
    val winSize = (1000, 1000)
    var t = 0
    
    val day = geometry.Constants.dt
    
    val system = new System()
    
    val planet = new Planet("testPlanet", 100, 1, new Vector(0,5500,0), new Vector(0,0,0), system)
    system.addBody(planet)
    
    val planet2 = new Planet("testPlanet2", 100, 1, new Vector(0,15,0), new Vector(0,0,0), system)
    system.addBody(planet2)
    
    val star = new Star("testStar", 100, 1, new Vector(0,0,0), new Vector(0,0,0), system)
    system.addBody(star)
    
    val plane = new Plane(0,0,1, -10)
    val fp = new geometry.Point(0,0,11)
    
    val camera = new Camera(plane, fp)
  
    
  def onKeyPress(keyCode: Value) = keyCode match {
    case Key.Plus    => camera.zoomIn()
    case Key.Minus   => camera.zoomOut()
    case otherKey => // do nothing
  }
    
    
  def onPaint(g: Graphics2D) {
    val img:BufferedImage = camera.capture(system)
    g.drawImage(img, null, 0, 0)
    g.setColor(Color.GREEN)
    
    val output = system.getDate
    val font = new Font("Bob", 12, 20)
    g.setFont(font)
    val back = g.getFontMetrics(font).stringWidth(output)
    
    g.drawString(output, winSize._1 - back - 8, winSize._2- 8)
    
  }  

  def top: MainFrame = new MainFrame{
    title = "Solar System"
    contents = mainPanel
  }
  
  
  var x = 0
  def mainPanel = new Panel {
    preferredSize = new Dimension( winSize._1, winSize._2 )
    focusable = true
    listenTo(keys)
    reactions += {
      case KeyPressed(_, key, _, _) =>
        onKeyPress(key)
        repaint
    }
    
    override def paint(g: Graphics2D) {
    
      onPaint(g)/*
      repaint()
      revalidate()
      */
      
    }
  }
}
  
