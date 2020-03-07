package gui


import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon

// This program is introduced in Chapter 12.3 of the ebook.

import swing._
import event._
import geometry._
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.geom.Dimension2D

import system._
import event.Key._
import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle}
import java.awt.{Color => AWTColor}
       

/*
 * Some swing things are 
 * 
 */
object SimulatorApp extends SimpleSwingApplication  {
  
       
    
    val winSize = (1000, 1000)
    
    val day = geometry.Constants.dt
    
    //val increment = 1 * day
    //val delaySec = 1
    
    val system:System = new System()
    
    val sun = new Star(
        "sun", 
        1.989e30, //Mass
        696340e3, //Radius
        new Vector(0, 0, 0),  //Position
        new Vector(0, 12.27, 0),  //Velocity
        system)
    
    val earth = new Planet(
      "earth", 
      5.972e24, 
      6371e3,
      new Vector(1.49598e11, 0, 0), 
      new Vector(0, 29.78e3, 0), 
      system)
    
    
    val mars = new Planet(
      "mars", 
      6.39e23, 
      3389.5e3,
      new Vector(2.27987e11, 0, 0), 
      new Vector(0, 24.1e3, 0), 
      system)
      
    
    val mercury = new Planet(
      "mercury", 
      3.285e23, 
      2439.7e3,
      new Vector(5.834e10, 0, 0), 
      new Vector(0, 48e3, 0), 
      system)
  
    val venus = new Planet(
      "venus", 
      4.867e24, 
      6051.8e3,
      new Vector(1.0815e11, 0, 0), 
      new Vector(0, 35.0e3, 0), 
      system)
    
    val jupiter = new Planet(
      "jupiter",
      1898.19e24 ,
      69911e3,
      new Vector(-7.7925e11,0,0),
      new Vector(0,-13.06e3,0),
      system)
    
    val saturn = new Planet(
      "saturn",
      9543e-7 ,
      2.85721e-4,
      new Vector(10.012,0,0),
      new Vector(0,2.042,0),
      system)
        /*
    val neptune = new Planet(
      "neptune",
      5.14e-5 ,
      1.6458787e-4,
      new Vector(29.8801,0,0),
      new Vector(0,1.1454,0),
      system)  
    
    val uranus = new Planet(
      "neptune",
      4.3645e-5 ,
      1.6953e-4,
      new Vector(19.8138,0,0),
      new Vector(0,1.43443,0),
      system)
    */
    
    system.addBody(sun)
    
    system.addBody(earth)
    system.addBody(mars)
    system.addBody(mercury)
    system.addBody(venus)
    system.addBody(jupiter)
    
    val initial = system.copy
    
    /*
    system.addBodysaturn)
    system.addBodyuranus)
    system.addBodyneptune)
    */
    
    
    val fps = 24  *3
    //5.9e7
    //11e11
    //1e8
    val d:Double = 5.9e7 //m
    val f:Double = 0.200 //m
    
    val camera = new Camera(
      new Plane(0,0,-1.0,d),
      new geometry.Point(0, 0, (d + f) ),
      new geometry.Vector(0,-1,0)
    )
    
    
    def onKeyPress(keyCode: Value) = keyCode match {
      case Key.Plus    => camera.zoomIn()
      case Key.Minus   => camera.zoomOut()
      case _ => // do nothing
    }
    
    
    //Test
    val pauseButton = new Button("Pause")
    val restartButton  = new Button("Restart"){
      reactions += {
        case clickEvent: ButtonClicked =>{
          restart
        }
      }
    }
    
    val prompt = new Label("Date: ")
    prompt.size.setSize(prompt.size.width * 2, prompt.size.height)
    
    val timeController = new BoxPanel(Orientation.Horizontal)
    timeController.contents += restartButton
    timeController.contents += pauseButton
    timeController.contents += prompt
    
    val topPanel = new scala.swing.MenuBar{
        contents += new Menu("File") {
          
          contents += new MenuItem( "New..." ){
            reactions += {
              case clickEvent: ButtonClicked =>{
                test
              }
            }
          }
          
          contents += new MenuItem( "Load..." ){
            reactions += {
              case clickEvent: ButtonClicked =>{
                load
              }
            }
          }
          
          contents += new MenuItem( "Save" )
          contents += new MenuItem( "Save as..." )
          contents += new MenuItem( "Exit" ){
            reactions += {
              case clickEvent: ButtonClicked =>{
                exit
              }
            }
          }
          
      }
        contents += new Menu("Simulation") {
          contents += new MenuItem( "Manage bodies" )
      }
        contents += new Menu("Display") {
          contents += new MenuItem( "Properties..." )
          contents += new MenuItem( "Camera settings... " )
          contents += new MenuItem( "Toggle vectors" )
      }
        contents += new Menu("Window") {
          mnemonic = Key.W
          contents += new MenuItem( "Show view..." )
      }
        contents += new Menu("Help") {
          contents += new MenuItem( "Instructions..." )
          contents += new MenuItem( "About Solar System" )
          contents += new MenuItem("test")
          
          
      }
    }
    
    
    
    val newSystem = new Dialog(){
      title = "Create a new simulation"
      //preferredSize = new Dimension(400, 400)
      
      contents = new scala.swing.BoxPanel(Orientation.Vertical){
        
        contents += new scala.swing.Label("Name")
        
        val bob = new scala.swing.PasswordField
        bob.maximumSize = new Dimension(200, 20)
        contents += bob
        contents += new scala.swing.Label("Start year")
        contents += new scala.swing.ComboBox(Seq.tabulate(2050)(_ + 1).drop(1969))
        contents += new scala.swing.Label("Start month")
        contents += new scala.swing.ComboBox(Seq.tabulate(12)(_ + 1).drop(0))
        contents += new scala.swing.Label("Start date")
        contents += new scala.swing.ComboBox(Seq.tabulate(31)(_ + 1).drop(0))
        
        contents += new scala.swing.Label("Name")
      }
    }
    
    def test = {
      newSystem.open()
      newSystem.background = Color.RED
      newSystem.centerOnScreen()
      newSystem.visible = true
    }
    
    def restart = {
      println("restarted")
      system.set(initial)
    }
      
    def exit = {
      //TODO Add grace. 
      System.exit(1)
    }
    
    def load = {
      //TODO Do something
      val bob  = new scala.swing.FileChooser
      val mike = bob.showOpenDialog(mainPanel);
    }
    
    
    
    val rightpanel = new scala.swing.FlowPanel(){
      preferredSize = new Dimension(300, 1000)
      background = Color.LIGHT_GRAY
      contents += new Label("hej")
      contents += new TextField("hej")
    }
    
    val midPanel = new FlowPanel()
    midPanel.contents += mainPanel
    midPanel.contents += rightpanel
    
    val wholePanel = new BoxPanel(Orientation.Vertical)
    wholePanel.contents += midPanel
    wholePanel.contents += timeController
    
    val allContent = new MainFrame{
      contents = wholePanel
      menuBar = topPanel
      title = "Solar system simulator"
    }
    

    
    def top = this.allContent
    
    this.listenTo(pauseButton)
    
    
    pauseButton.reactions += {
      case clickEvent: ButtonClicked =>{
        isPaused = !isPaused
        pauseButton.text = isPaused match{
          case true => "Play"
          case false => "Pause"
        }
      }
    }

  var x = 0
  
  var isPaused = false
  
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
      //printOutput()
      
      if(!isPaused){
        system.update
      }
      
      val img:BufferedImage = camera.capture(system)
      g.drawImage(img, null, 0, 0)
      g.setColor(Color.GREEN)
      
      val output = system.getDate
      val font = new Font("Bob", 12, 20)
      g.setFont(font)
      val back = g.getFontMetrics(font).stringWidth(output)
      
      g.drawString(output, winSize._1 - back - 8, winSize._2- 8)
      prompt.text = output
        
      Thread.sleep(1000/fps)
      repaint()
      revalidate()
      
      
    }
  }
  
  var i = 0
  def printOutput(){
    println( i + "," +  (sun.location).magnitude)
    i+=1
  }
  
}
  
