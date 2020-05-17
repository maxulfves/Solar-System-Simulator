package gui
import javax.swing._
import java.awt._
import java.awt.BorderLayout
import scala.io.Source

object HelpWindow extends JPanel {
  setLayout(new BorderLayout())
  
  val str = Source.fromFile(geometry.Constants.instructionsFile).mkString
  val test = new JLabel(str)
  val bob = new JScrollPane(test)
  
  this.setPreferredSize(new Dimension(320, 200))
  test.setPreferredSize(new Dimension(300, 940))
  
  val fontx = new Font(null, java.awt.Font.PLAIN, 10);    
  test.setFont(fontx)
  //bob.add(test)
  
  add(bob, BorderLayout.CENTER)
  
  
}