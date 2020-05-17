package gui
import scala.swing._
import scala.swing.event._
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.awt.event.MouseWheelEvent

object simulationCanvasPanel extends Panel {

  //preferredSize = new Dimension((winSize._1 * 0.7).toInt, (winSize._2 * 0.95).toInt)
  focusable = true
  
  var fps = 60
  var sps = 60
  
  listenTo(keys)
  //Focus is lost when another component is clicked. Therefore we need to regain focus when this component is clicked.
  listenTo(mouse.clicks)
  listenTo(mouse.wheel)

  reactions += {
    case MouseClicked(_, event, _, _, _) => this.requestFocus()
    case e: MouseWheelMoved => {

      if (e.rotation > 0) {
        //SimulatorApp.camera.zoomOut
        SimulatorApp.camera.moveOut
      } else {
        //SimulatorApp.camera.zoomIn
        SimulatorApp.camera.moveIn
      }
    }
    case KeyPressed(_, key, _, _) => SimulatorApp.onKeyPress(key)
  }

  /** The thread that updates the simulation's state. */
  private object SimulationThread extends Thread {
    override def run() = {
      while (true) {
        //Updates the simulation if isPaused is false.
        if (!SimulatorApp.getIsPaused) {
          SimulatorApp.system.update;
          TimeController.setDate(SimulatorApp.system.getDate)

          //Add graph data to the GraphData graph.
          if (SimulatorApp.tracking && SimulatorApp.system.bodies.length > 0) {
            SimulatorApp.addGraphData();
          }
        }
        //TODO editable
        Thread.sleep((1000.0/sps).toLong)
      }
    }
  }

  SimulationThread.start()
  private var image: BufferedImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB)

  /** A separate thread that updates the image*/
  private object RenderingTread extends Thread {
    override def run() = {
      while (true) {
        //val myImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB)
        //val g = myImg.createGraphics()

        //Uses a camera to 'capture' a part of the system.
        if (size.width > 0 && size.height > 0) {
          val img: BufferedImage = SimulatorApp.camera.capture(SimulatorApp.system, size.width, size.height)
          image = img
        }
            
        SimulatorApp.guiTickCounter += 1
        //Pauses the execution so that every frame is of a certain predefined length.
        Thread.sleep((1000.0 / fps).toInt)
        
      }
    }
  }
  RenderingTread.start()

  //A swing method that updates the component's canvas.
  override def paint(g: Graphics2D) {
    g.drawImage(image, null, 0, 0)

    //If the user is looking at the Bodies-page in the rightside menu ---> update form data.
    if (gui.RightPanel.selection.page.title == "Bodies" && !SimulatorApp.getIsPaused) RightPanel.bodiesPanel.updateSelectionData()


    //Repaint and update the window.
    repaint()
    revalidate()
  }

}