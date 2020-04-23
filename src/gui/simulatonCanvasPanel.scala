package gui
import scala.swing._
import scala.swing.event._
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.awt.event.MouseWheelEvent

object simulationCanvasPanel extends Panel {

  //preferredSize = new Dimension((winSize._1 * 0.7).toInt, (winSize._2 * 0.95).toInt)
  focusable = true

  listenTo(keys)
  //Focus is lost when another component is clicked. Therefore we need to regain focus when this component is clicked.
  listenTo(mouse.clicks)
  listenTo(mouse.wheel)
  
  reactions += {
    case MouseClicked(_, event, _, _, _) => this.requestFocus()
    case e:MouseWheelMoved => {
  
      if(e.rotation > 0){
        //SimulatorApp.camera.zoomOut
        SimulatorApp.camera.moveOut
      }else{
        //SimulatorApp.camera.zoomIn     
        SimulatorApp.camera.moveIn
      }
    }
    case KeyPressed(_, key, _, _)        => SimulatorApp.onKeyPress(key)
  }
  
  //A swing method that updates the component's canvas.
  override def paint(g: Graphics2D) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

    //Updates the simulation if isPaused is false.
    if (!SimulatorApp.getIsPaused) {
      SimulatorApp.system.update;
      TimeController.setDate(SimulatorApp.system.getDate)

      //Add graph data to the GraphData graph.
      if (SimulatorApp.tracking && SimulatorApp.system.bodies.length > 0) {
        SimulatorApp.addGraphData();
      }
    }

    //Uses a camera to 'capture' a part of the system.
    val img: BufferedImage = SimulatorApp.camera.capture(SimulatorApp.system, this.size.getWidth.toInt, this.size.getHeight.toInt)
    g.drawImage(img, null, 0, 0)

    //If the user is looking at the Bodies-page in the rightside menu ---> update form data.
    if (gui.RightPanel.selection.page.title == "Bodies" && !SimulatorApp.getIsPaused) RightPanel.updateSelectionData()

    //Pauses the execution so that every frame is of a certain predefined length.
    Thread.sleep((1000.0 / SimulatorApp.fps).toInt)
    SimulatorApp.guiTickCounter += 1

    //Repaint and update the window.
    repaint()
    revalidate()
  }

}