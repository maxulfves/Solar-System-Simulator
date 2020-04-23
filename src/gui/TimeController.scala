package gui

import scala.swing._
import scala.swing.event._
import java.awt.Color

object TimeController extends GridBagPanel {
  background = Color.GRAY

  def constraints(x: Int, y: Int,
                  gridwidth: Int = 1, gridheight: Int = 1,
                  weightx: Double = 0.0, weighty: Double = 0.0,
                  fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None): Constraints = {
    val c = new Constraints
    c.gridx = x
    c.gridy = y
    c.gridwidth = gridwidth
    c.gridheight = gridheight
    c.weightx = weightx
    c.weighty = weighty
    c.fill = fill
    c
  }

  val pauseButton = new Button("Pause") {
    reactions += {
      case clickEvent: ButtonClicked => gui.SimulatorApp.togglePause
    }
    override def paint(g: Graphics2D) {
      super.paint(g)
      this.text = gui.SimulatorApp.getIsPaused match {
        case true  => "Play"
        case false => "Pause"
      }
      
      repaint
    }
  }

  val restartButton = new Button("Restart") {
    reactions += {
      case clickEvent: ButtonClicked => {
        gui.SimulatorApp.restart
      }
    }
  }

  val dateLabel = new Label("Date: ")
  val weightx = 1
  val weighty = 1
  val gridwidth = 1
  val gridheight = 1

  add(restartButton, constraints(1, 0, gridwidth, gridheight, weightx, weighty, GridBagPanel.Fill.Both))
  add(pauseButton, constraints(0, 0, gridwidth, gridheight, weightx, weighty, GridBagPanel.Fill.Both))
  add(dateLabel, constraints(2, 0, gridwidth, gridheight, weightx, weighty, GridBagPanel.Fill.Both))

  def setDate(arg: String) = {
    dateLabel.text = arg
  }
}