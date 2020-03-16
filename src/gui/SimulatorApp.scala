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
import javax.swing.JList
import scala.collection.mutable.Buffer
       

/*
 * Some swing things are 
 * 
 */
object SimulatorApp extends SimpleSwingApplication  {
  
       
    
    val winSize = (1000, 1000)
    
    val day = geometry.Constants.dt
    
    val system:System = new System()
    
    val sun = new Star(
        "sun", 
        1.989e30, //Mass
        696340e3, //Radius
        new Vector(0, 0, 0),  //Position
        new Vector(0.001, 12.27, 0.001),  //Velocity
        system)
    
    val earth = new Planet(
      "earth", 
      5.972e24, 
      6371e3,
      new Vector(1.49598e11, 0, 0.01), 
      new Vector(0, 29.78e3, 0.0), 
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
      5.68e26 ,
      58232e3,
      new Vector(14.972e11, 0, 0),
      new Vector(0,9.68e3,0),
      system)
    
    system.addBody(sun)
    
    system.addBody(earth)
    system.addBody(mars)
    system.addBody(mercury)
    system.addBody(venus)
    system.addBody(jupiter)
    system.addBody(saturn)
    
    val initial = system.copy
    
    
    val fps = 24 * 3
    
    // 5.96e8  --- 2
    // 2.29e12 --- 0.030
    
    val d:Double = 1.5e11  //m
    val f:Double = 2.0    //m
    
    /*
    val camera = new Camera(
      new Plane(0,0,-1.0,d),
      new geometry.Point(0, 0, (d + f) ),
      new geometry.Vector(0,-1,0)
    )*/
    
    //println("-----")
    
    val plane = new Plane(-math.sqrt(2), 0, -math.sqrt(2), d * 2 )
    
    val camera = new Camera(
      new Plane(0, 0, 1.0,d),
      new geometry.Point(0, 0, -(d + f) ),
      new geometry.Vector(0,1,0)
    )
    
    val angle = (math.Pi * 2.0) / 1000
    def onKeyPress(keyCode: Value) = keyCode match {
      case Key.Plus    => camera.zoomIn
      case Key.Minus   => camera.zoomOut
      case Key.Up      => camera.rotateBy(angle, 0, 0)
      case Key.Down    => camera.rotateBy(-angle, 0, 0)
      case Key.Left    => camera.rotateBy(0, angle, 0)
      case Key.Right   => camera.rotateBy(0, -angle, 0)
      case Key.Period  => camera.rotateBy(0, 0, angle)
      case Key.Comma   => camera.rotateBy(0, 0, -angle) 
      case Key.Space   => togglePause 
      case Key.R   => restart 
      case _ => // do nothing
    }
    
    camera.rotateBy(0, 0, 0)
    
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
          contents += new MenuItem( "Manage bodies" ){
            reactions += {
              case clickEvent: ButtonClicked => manageBodies()
            }
          }
      }
        contents += new Menu("Display") {
          contents += new MenuItem( "Properties..." )
          contents += new MenuItem( "Camera settings... " )
          contents += new MenuItem( "Toggle vectors" )
      }
        contents += new Menu("Window") {
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
    
    

    
    def manageBodies(){
      val bodiesWindow = new scala.swing.Dialog(){
        title = "bodies"
        contents = new scala.swing.FlowPanel{
          
          val str_bodies = system.bodies.map(b => (b.getName, b))
          
          
          val lw =  new scala.swing.ListView(str_bodies.map(b => b._1))
          
          val tw_name = new TextField()
          val tw_mass = new TextArea()
          
          //Location
          val lb_x = new Label("Location X")
          val lb_y = new Label("Location Y")
          val lb_z = new Label("Location Z")
          
          val tw_x = new TextField
          val tw_y = new TextField
          val tw_z = new TextField
          
          //Velocity
          val lb_vel_x = new Label("Velocity X")
          val lb_vel_y = new Label("Velocity Y")
          val lb_vel_z = new Label("Velocity Z")
          
          val tw_vel_x = new TextField
          val tw_vel_y = new TextField
          val tw_vel_z = new TextField
          
          //Control buttons
          val btn_save = new Button("Save"){
            reactions += {
              case action: ButtonClicked => {
                selection.setMass(tw_mass.text.toDouble)
                println("updated")
              }
              
            }            
          }
          
          val btn_remove = new Button("Remove"){
            reactions += {
              case action: ButtonClicked => {
                println("hej")
                system.remove(selection)
              }
              
            }
          }
          
          listenTo(btn_save)
          
          var selection:Body = system.bodies(0)
          
          listenTo(lw.selection)
          
          
          reactions += {
            case SelectionChanged(`lw`) => {
              selection = str_bodies.filter(_._1 == lw.selection.items(0)).head._2
              tw_name.text = lw.selection.items(0)
              tw_mass.text = selection.getMass + " kg"
              
              tw_x.text = selection.location.x + ""
              tw_y.text = selection.location.y + ""
              tw_z.text = selection.location.z + ""
              
              tw_vel_x.text = selection.velocity.x + ""
              tw_vel_y.text = selection.velocity.y + ""
              tw_vel_z.text = selection.velocity.z + ""
              
            }
            
          }
          
          val bp = new BoxPanel(Orientation.Vertical){
            contents += new Label("Name")
            contents += tw_name
            contents += new Label("Mass")
            contents += tw_mass
            
            //Location
            contents += lb_x
            contents += tw_x
            contents += lb_y
            contents += tw_y
            contents += lb_z
            contents += tw_z
            
            //Velocity
            contents += lb_vel_x
            contents += tw_vel_x
            contents += lb_vel_y 
            contents += tw_vel_y
            contents += lb_vel_z
            contents += tw_vel_z
            
            
            //Controll buttons
            contents += btn_save
            contents += btn_remove
          }
          
          
          contents += lw
          contents += bp
          
        }
        
      }
      
      
      bodiesWindow.open()
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
    
    
    val rightpanel = new scala.swing.TabbedPane(){
      preferredSize = new Dimension(300, 1000)
      background = Color.LIGHT_GRAY
    }
    
       
    
    rightpanel.pages.addOne(new TabbedPane.Page("Bodies", new BoxPanel(Orientation.Vertical){
      val str_bodies = system.bodies.map(b => (b.getName, b))
      
      val lw =  new scala.swing.ListView(str_bodies.map(b => b._1))
      
      
      val tw_name = new TextField()
      val tw_mass = new TextField()
          
      //Location
      val lb_x = new Label("Location X")
          val lb_y = new Label("Location Y")
          val lb_z = new Label("Location Z")
          
          val tw_x = new TextField
          val tw_y = new TextField
          val tw_z = new TextField
          
          //Velocity
          val lb_vel_x = new Label("Velocity X")
          val lb_vel_y = new Label("Velocity Y")
          val lb_vel_z = new Label("Velocity Z")
          
          val tw_vel_x = new TextField
          val tw_vel_y = new TextField
          val tw_vel_z = new TextField
          
          //Control buttons
          val btn_save = new Button("Save"){
            reactions += {
              case action: ButtonClicked => {
                selection.setMass(tw_mass.text.toDouble)
                println("updated")
              }
              
            }            
          }
          val btn_remove = new Button("Remove"){
            reactions += {
              case action: ButtonClicked => {
                system.remove(selection)
              }
              
            }
          }
          
          var selection:Body = system.bodies(0)
          
          listenTo(lw.selection)
          
          
          reactions += {
            case SelectionChanged(`lw`) => {
              selection = str_bodies.filter(_._1 == lw.selection.items(0)).head._2
              tw_name.text = lw.selection.items(0)
              tw_mass.text = selection.getMass.toString()
              
              tw_x.text = selection.location.x + ""
              tw_y.text = selection.location.y + ""
              tw_z.text = selection.location.z + ""
              
              tw_vel_x.text = selection.velocity.x + ""
              tw_vel_y.text = selection.velocity.y + ""
              tw_vel_z.text = selection.velocity.z + ""
              
            }
          }
          
          val bp = new BoxPanel(Orientation.Vertical){
            contents += tw_name
            contents += tw_mass
            
            //Location
            contents += lb_x
            contents += tw_x
            contents += lb_y
            contents += tw_y
            contents += lb_z
            contents += tw_z
            
            //Velocity
            contents += lb_vel_x
            contents += tw_vel_x
            contents += lb_vel_y 
            contents += tw_vel_y
            contents += lb_vel_z
            contents += tw_vel_z
            
            
            //Control buttons
            contents += btn_save
            contents += btn_remove
            

            for(i <- contents){
              val dim = i.preferredSize
              dim.width = i.maximumSize.width
              i.maximumSize = dim
              
            }
            
          }
          lw.selectIndices(0)
          lw.fixedCellWidth = 300
          
          
          contents += lw
          contents += bp
          

      
      
    }))
    
    
    rightpanel.pages.addOne(new TabbedPane.Page("Camera settings", new ScrollPane(){
      val rotX = new TextField(camera.rotationX + "")
      val rotY = new TextField(camera.rotationY + "")
      val rotZ = new TextField(camera.rotationZ + "")
      val fLen = new TextField(camera.fLen2 + "")
      val dis = new TextField(camera.plane.D + "")
      
      contents = new BoxPanel(Orientation.Vertical){
          contents += new Label("Distance: ")
          contents += dis
          
          contents += new Label("Focal length")
          contents += fLen
          
          contents += new Label("Rotation X: ")
          contents += rotX
          
          contents += new Label("Rotation Y: ")
          contents += rotY
          
          contents += new Label("Rotation Z: ")
          contents += rotZ
          
          contents += new Button("Update"){
            reactions += {
              case action: ButtonClicked => {
                camera.rotateTo(rotX.text.toDouble/360*(math.Pi*2), rotY.text.toDouble/360*(math.Pi*2), rotZ.text.toDouble/360*(math.Pi*2/360))
                camera.setFocalLength(fLen.text.toDouble)
                camera.plane.D = dis.text.toDouble
              }
            }
          }
          
          for(i <- contents){
            val dim = i.preferredSize
            dim.width = i.maximumSize.width
            i.maximumSize = dim
            
          }
          
        }
    }))
    
    
    rightpanel.pages.addOne(new TabbedPane.Page("Simulation", new BoxPanel(Orientation.Horizontal)))
    
    
        
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
      case clickEvent: ButtonClicked =>togglePause
    }

  var x = 0
  
  
  
  private var isPaused = false
  
  def togglePause = {
    isPaused = !isPaused
    pauseButton.text = isPaused match{
      case true => "Play"
      case false => "Pause"
    }
  }
  
  
  def mainPanel = new Panel {
    preferredSize = new Dimension( winSize._1, winSize._2 )
    focusable = true
    listenTo(keys)
    reactions += {
      case KeyPressed(_, key, _, _) =>
        onKeyPress(key)
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
      val rotX = "X: " + camera.rotationX / (2 * math.Pi) * 360
      val rotY = "Y: " + camera.rotationY / (2 * math.Pi) * 360
      val rotZ = "Z: " + camera.rotationZ / (2 * math.Pi) * 360
      
      
      val font = new Font("Bob", 12, 20)
      g.setFont(font)
      val back = g.getFontMetrics(font).stringWidth(output)
      val txtHeight = g.getFontMetrics(font).getHeight
      
      
      
      g.drawString(output, winSize._1 - back - 8, winSize._2 - 10)
      
      g.drawString(camera.focalLength + "f", winSize._1 - back - 8, winSize._2 - txtHeight *6)
      g.drawString(rotX, winSize._1 - back - 8, winSize._2 - txtHeight *5)
      g.drawString(rotY, winSize._1 - back - 8, winSize._2 - txtHeight *4)
      g.drawString(rotZ, winSize._1 - back - 8, winSize._2 - txtHeight * 3)
      
      
      prompt.text = output
      
      time += 1
      
      Thread.sleep(1000/fps)
      repaint()
      revalidate()
      
      /*
      println(time + "," + mercury.location.x)
      println(time + "," + mercury.location.y)*/
    }
    
    var time = 0
  }
  
  
  
  
  var i = 0
  def printOutput(){
    i+=1
  }
  
}
  
