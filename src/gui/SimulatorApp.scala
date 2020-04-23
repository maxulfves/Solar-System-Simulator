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
import java.awt.{ Dimension, Graphics2D, Graphics, Image, Rectangle }
import java.awt.{ Color => AWTColor }
import javax.swing.JList
import scala.collection.mutable.Buffer
import files.Serializer
import java.io.File
import java.io.FileFilter
import java.io.FilenameFilter
import javax.swing.filechooser.FileNameExtensionFilter
import java.awt.GraphicsConfiguration
import java.util.Calendar
import javax.swing.Box
import java.awt.event.KeyListener
import scala.swing.event.KeyPressed
import scala.swing.event.KeyReleased
import javax.swing.DefaultComboBoxModel
import java.awt.Toolkit
import java.awt.RenderingHints
import javax.swing.JOptionPane

object SimulatorApp extends SimpleSwingApplication {
  
  
  /** Determins if graph should be tracking something. */  
  var tracking:Boolean = false
  var openedFile:Option[File] = None
  var isExample:Boolean = false
  
  val sc = Toolkit.getDefaultToolkit().getScreenSize();
  val winSize = ((sc.getWidth*0.7).toInt,  (sc.getHeight*0.7).toInt)
  
  /** Counts ticks passed since program start. */
  var guiTickCounter = 0
    
  /** A variable used to control if the project is paused. */
  private var isPaused = true
  
  val mySerializer = new files.Serializer()
  var system:System = null//mySerializer.deserialize(openedFile)
  var initial:System = null//system.copy
  var selectedBody: Body = null//system.bodies(0)
  
  val fps = Constants.fps
  

  val camera = new Camera()

  

  


  val angle = 1.toRadians
  
  def onKeyPress(keyCode: Value) = {
    
    keyCode match {
      case Key.Plus   => camera.zoomIn
      case Key.Minus  => camera.zoomOut
      case Key.Up     => camera.rotateBy(-angle, 0, 0)
      case Key.Down   => camera.rotateBy(angle, 0, 0)
      case Key.Left   => camera.rotateBy(0, 0, angle)
      case Key.Right  => camera.rotateBy(0, 0, -angle)
      case Key.Period => camera.rotateBy(0, angle, 0)
      case Key.Comma  => camera.rotateBy(0, -angle, 0)
      case Key.Space  => togglePause
      case Key.R      => restart
      case _          => // do nothing
    }
    
    //RightPanel.cameraPanel.update
  }
  
  
  
  
  def graphReady = {
    

    val ret = (graphData.bodyA.isDefined && 
      graphData.field.isDefined && 
      (graphData.bodyB.isDefined  || graphData.xTime) &&
      (graphData.fieldB.isDefined || (graphData.field.head == "Distance to..."|| graphData.xTime)) )
      
      
      ret
  }
      
  
  /** Adds data to the graph in Graph-page. */
  def addGraphData() {
    require(system.bodies.size > 0)
    var x = 0.0
    var y = 0.0

    
    //val dat = system.bodies(1).location.x

    var distanceTo = false

    graphData.bodyA match {
      case None =>
      case Some(a) => {
        graphData.field match {
          case Some(fieldA) => {
            fieldA match {
              case "X"              => y = a.location.x
              case "Y"              => y = a.location.y
              case "Z"              => y = a.location.z
              case "Velocity X"     => y = a.velocity.x
              case "Velocity Y"     => y = a.velocity.y
              case "Velocity Z"     => y = a.velocity.z
              case "Distance to..." => distanceTo = true

            }
          }
          case None => return
        }
      }
    }

    graphData.bodyB match {
      case None => //Handled elsewhere
      case Some(a) => {
        graphData.fieldB match {
          case Some(fieldB) => {
            fieldB match {
              case "X"          => x = a.location.x
              case "Y"          => x = a.location.y
              case "Z"          => x = a.location.z
              case "Velocity X" => x = a.velocity.x
              case "Velocity Y" => x = a.velocity.y
              case "Velocity Z" => x = a.velocity.z
            }
          }
          case None => 
        }
      }
    }
    
    
    if (distanceTo && (graphData.bodyA.isDefined && graphData.bodyB.isDefined)) {
      y = graphData.bodyA.head.location.toPoint().distanceTo(graphData.bodyB.head.location.toPoint())
    }

    if (graphData.xTime) {
      graph.addData(system.getTime, y)
    } else {
      graph.addData(x, y)
    }

  }

  object graphData {
    var bodyA: Option[Body] = None
    var bodyB: Option[Body] = None
    var field: Option[String] = None
    var fieldB: Option[String] = None
    var xTime: Boolean = true
  }

  
  
  //val result = JOptionPane.showMessageDialog(null, HelpWindow, "Instructions", JOptionPane.PLAIN_MESSAGE);
  //val newSystem: Dialog = NewSystemDialog
  
  def openNewSystemDialog = {
    //val test = JOptionPane.showMessageDialog(null, NewSystemDialog, NewSystemDialog.title, JOptionPane.DEFAULT_OPTION)
    //val result = JOptionPane.showOptionDialog(null, NewSystemDialog, "", JOptionPane.DEFAULT_OPTION, JOptionPane.DEFAULT_OPTION, null, Array(), null)
    
    NewSystemDialog.showNewSystemDialog() match{
      case Some(createdSystem) => {
        isExample = false 
        openedFile = None
        setSystem(createdSystem)
      }
      case None => //Cancel was pressed.
    }
    
    
    //NewSystemDialog.createDialog(MainFrame.peer, "Title").show()
    
    //NewSystemDialog.setVisible(true);
    //JOptionPane.showMessageDialog(null, NewSystemDialog)
    
    //newSystem.open()
    //newSystem.centerOnScreen()
    
    //newSystem.visible = true
  }

  /**Restarts the simulation from the previous load.*/
  def restart = {
    println("restarted")
    system = initial.copy
    RightPanel.lw.listData = system.bodies.map(b => b.getName)
    graph.clear
    tracking = false
  }

  /**Called when user closes app from menu. 	 */
  def exit = {
    System.exit(1)
  }

  def save = {
    openedFile match {
      case Some(file) => mySerializer.serialize(system, file)
      case None => //Shouldn't happen
    }
  }

  def load = {
    println("load")
    val fileDialog = new scala.swing.FileChooser(new File(Constants.simulationFileDirectory))
    fileDialog.title = "Load a system"
    fileDialog.fileFilter = new FileNameExtensionFilter("Solarsystem state (.%s)".format(Constants.extension), Constants.extension)
    fileDialog.multiSelectionEnabled = false
    fileDialog.controlButtonsAreShown = true
    fileDialog.fileHidingEnabled = false

    val selection = fileDialog.showOpenDialog(simulationCanvasPanel);
    if (selection == FileChooser.Result.Approve) {
      val fileToLoad: File = fileDialog.selectedFile
      loadFile(fileToLoad);
    }
    
    
  }
  
  def loadFile(fileToLoad:File) {
    
    println("Load file: " + fileToLoad.getAbsolutePath());
    isExample = fileToLoad.getParentFile.getAbsolutePath == new File(Constants.exampleDirectory).getAbsolutePath
    
    openedFile = Some(fileToLoad)

    //TODO Alert if operation failed.
    //TODO Check if file is legit
    setSystem((mySerializer.deserialize(fileToLoad)))
    
  }
  
  
  def setSystem(sys:System){
    
    camera.rotateBy(0,0,0)
    system = sys
    initial = system.copy
    
    //Set the listview to current system.
    RightPanel.updateSystem()
    
    graph.clear
    togglePause(true)
  }
  
  
  def saveAs = {
    val fileDialog = new scala.swing.FileChooser(new File(Constants.simulationFileDirectory))
    
    fileDialog.title = "Save as..."
    fileDialog.fileFilter = new FileNameExtensionFilter("Solarsystem state (.%s)".format(Constants.extension), Constants.extension)
    fileDialog.multiSelectionEnabled = false
    fileDialog.controlButtonsAreShown = true
    fileDialog.fileHidingEnabled = false

    val defaultName = system.getName + "_" + system.getDate.replace(' ', '_') + "." + Constants.extension

    fileDialog.selectedFile = new File(defaultName)

    val selection = fileDialog.showSaveDialog(simulationCanvasPanel);

    if (selection == FileChooser.Result.Approve) {
      val fileToSave: File = fileDialog.selectedFile
      println("Save as file: " + fileToSave.getAbsolutePath());
      mySerializer.serialize(system, fileToSave)
      //TODO Alert if operation failed.
    }
  }

  val graph = new Graph()
  
  
  
  
  //Gridbag idea source: http://otfried.org/scala/index_42.html
  def top: MainFrame = gui.MainFrame
  
  
  loadFile(new File(Constants.exampleSource))
  
  
  def getIsPaused = isPaused

  /**Toggles isPaused.*/
  def togglePause = {
    isPaused = !isPaused
    RightPanel.disable(isPaused)
  }
  
  
  def togglePause(arg:Boolean) = {
    isPaused = arg
    RightPanel.disable(isPaused)
  }

}
  
