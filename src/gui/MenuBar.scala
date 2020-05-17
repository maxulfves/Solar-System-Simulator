package gui

import scala.swing._
import scala.swing.event.ButtonClicked
import java.io.File
import javax.swing.JOptionPane
import javax.swing.JTextField
import javax.swing.JPasswordField
import javax.swing.JComponent
import javax.swing.JLabel
import scala.collection.mutable.Buffer
import javax.swing.JPanel
import java.awt.BorderLayout
import scala.io.Source

object MenuBar extends scala.swing.MenuBar {

  contents += new Menu("File") {
    contents += new MenuItem("New...") {
      reactions += {
        case clickEvent: ButtonClicked => {
          gui.SimulatorApp.openNewSystemDialog
        }
      }
    }

    contents += new MenuItem("Load...") {
      reactions += {
        case clickEvent: ButtonClicked => gui.SimulatorApp.load
      }
    }

    contents += new MenuItem("Save") {
      override def paint(g: Graphics2D) {
        super.paint(g)
        println("hello")
        this.enabled = !gui.SimulatorApp.isExample && gui.SimulatorApp.openedFile.isDefined
        repaint
        revalidate
      }
      reactions += {
        case clickEvent: ButtonClicked => {
          if (!gui.SimulatorApp.isExample) {
            gui.SimulatorApp.save
          } else {

            Dialog.showMessage(this, "You shouldn't overwrite an example file! \nTry \"Save as...\" instead. ", "Warning!", Dialog.Message.Warning)
          }
        }
      }
    }

    contents += new MenuItem("Save as...") {
      reactions += {
        case clickEvent: ButtonClicked => gui.SimulatorApp.saveAs
      }
    }

    contents += new MenuItem("Exit") {
      reactions += {
        case clickEvent: ButtonClicked => gui.SimulatorApp.exit
      }
    }

  }

  contents += new Menu("Simulation") {
    val files = new File("resources/examples").listFiles
    contents += new Menu("Examples") {
      for (file <- files) {
        contents += new MenuItem(file.getName) {
          reactions += {
            case clickEvent: ButtonClicked => gui.SimulatorApp.loadFile(file)
          }
        }
      }
    }
    contents += new MenuItem("Toggle vectors") {
      reactions += {
        case c: ButtonClicked => gui.SimulatorApp.camera.toggleVectots
      }
    }
    contents += new MenuItem("Toggle stars") {
      reactions += {
        case c: ButtonClicked => gui.SimulatorApp.camera.toggleStars
      }
    }
    contents += new MenuItem("Alter timestep") {
      reactions += {
        case c: ButtonClicked => {
          try {
            val nn = JOptionPane.showInputDialog(
              null,
              "Warning: It's not recommended to change the timestep in the middle of a \nsimulation as it will produce an inexact result.", gui.SimulatorApp.system.getStepSize);
            if (nn.toLong >= 1 && nn.toLong <= 2592000) {
              gui.SimulatorApp.system.setStepSize(nn.toLong)
            } else {
              JOptionPane.showMessageDialog(null, "Must be in range [1, 2592000]")
            }
          } catch {
            case e: NumberFormatException => JOptionPane.showMessageDialog(null, "Bad input!")
          }
        }
      }
    }
  }

  contents += new Menu("Help") {
    contents += new MenuItem("Instructions...") {
      reactions += {
        case c: ButtonClicked => {
          //JOptionPane.showMessageDialog(nullPeer(parent), message, title, messageType.id, Swing.wrapIcon(icon))

          val result = JOptionPane.showMessageDialog(
            null, HelpWindow, "Instructions", JOptionPane.PLAIN_MESSAGE);

          //HelpWindow.open()

        }
      }
    }

    contents += new MenuItem("About Solar System") {
      reactions += {
        case c: ButtonClicked => Dialog.showMessage(
          this,
          "Solar System Simulator was created by\n"
            + "Max Ulfves in Spring 2020"
            + "", "About", Dialog.Message.Info)
      }
    }
  }
}