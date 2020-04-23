package gui
import scala.swing._
import gui.SimulatorApp._

object MainFrame extends scala.swing.MainFrame{
  preferredSize = new Dimension(winSize._1, winSize._2)
  minimumSize = new Dimension(winSize._1/2, winSize._2/2)
  
  title = "Solar system simulator"
  
  iconImage = toolkit.getImage("icon.ico") 

  
  contents = new GridBagPanel {
  def constraints(x: Int, y: Int, width: Int = 1, height: Int = 1, weightx: Double = 0.0, weighty: Double = 0.0, fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None): Constraints = {
    val constraint = new Constraints
    constraint.gridx = x
    constraint.gridy = y
    constraint.gridwidth = width
    constraint.gridheight = height
    constraint.weightx = weightx
    constraint.weighty = weighty
    constraint.fill = fill
    constraint
  }
    
    add(RightPanel,  constraints(1, 0, height = 5, fill=GridBagPanel.Fill.Both))
    add(simulationCanvasPanel, constraints(0, 0, height = 3, width = 1, weighty = 1.0, weightx=1.0 , fill = GridBagPanel.Fill.Both))
    add(TimeController,constraints(0, 4, width = 1, fill = GridBagPanel.Fill.Both))
    
  }
  centerOnScreen()
  menuBar = gui.MenuBar
}