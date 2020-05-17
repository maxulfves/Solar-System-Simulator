package gui
import system._
import java.util.Calendar
import java.awt._
import javax.swing._
import java.util.Date
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.JobAttributes.DialogType

object NewSystemDialog extends JOptionPane {
  val title = "Create a new simulation"

  private val gbag = new java.awt.GridBagLayout
  private var index = 0
  setLayout(gbag)

  private def addRow(items: Vector[JComponent]) {
    val constraint = new GridBagConstraints()
    constraint.gridx = 0
    constraint.gridy = index
    constraint.gridwidth = 120 / items.size
    constraint.gridheight = 1
    constraint.weightx = 0
    constraint.weighty = 0
    constraint.ipady = 4
    constraint.fill = GridBagConstraints.HORIZONTAL

    for (item <- items) {
      add(item, constraint)
      constraint.gridx += 120 / items.size
    }
    index += 1
  }

  private val nameInput = new JTextField("New system")

  private val months: Array[Object] = Array("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  private val monthInput = new JSpinner(new SpinnerListModel(months));
  monthInput.setEditor(new JSpinner.DefaultEditor(monthInput))

  private val yearInput = new JSpinner(new SpinnerNumberModel(1970, 1970, 2100, 1));
  yearInput.setEditor(new JSpinner.NumberEditor(yearInput, "#"));

  private val dayInput = new JSpinner(new SpinnerNumberModel(1, 1, 31, 1));
  dayInput.setEditor(new JSpinner.DefaultEditor(dayInput));

  private val stepInput = new JSpinner(new SpinnerNumberModel(3600, 10, 2592000, 1));
  stepInput.setEditor(new JSpinner.NumberEditor(stepInput, "#"));

  private val lengthInput = new JSpinner(new SpinnerNumberModel(3600 * 24 * 365, 3600*24*7, 3e10, 1));
  lengthInput.setEditor(new JSpinner.NumberEditor(lengthInput, "#"));

  private val cancel = new JButton("Cancel")
  private val create = new JButton("Create")

  this.removeAll()
  addRow(Vector(new JLabel("Name: "), nameInput))
  addRow(Vector(new JLabel("Year: "), yearInput))
  addRow(Vector(new JSeparator))

  addRow(Vector(new JLabel("Month: "), monthInput))
  addRow(Vector(new JLabel("Day: "), dayInput))
  addRow(Vector(new JSeparator))

  addRow(Vector(new JLabel("Simulation length (s): "), lengthInput))
  addRow(Vector(new JLabel("Stepsize (s): "), stepInput))
  addRow(Vector(new JSeparator))

  addRow(Vector(cancel, create))

  private val dialog = createDialog(MainFrame.peer, title)
  private var result: Option[System] = None

  cancel.addActionListener(new ActionListener() {
    def actionPerformed(ev: ActionEvent) {
      result = None
      dialog.setVisible(false)
    }
  });

  create.addActionListener(new ActionListener() {
    def actionPerformed(ev: ActionEvent) {
      try {
        val name = nameInput.getText
        val length = lengthInput.getValue.asInstanceOf[Number].longValue()
        val stepsize = stepInput.getValue.asInstanceOf[Number].longValue()

        require((name.length >= 1 && name.length <= 256), "Name must be in range [1, 256]!")
        require( length > stepsize, "Length must be greater than stepsize!" )

        val system = new System(name, stepsize, length)

        val date = Calendar.getInstance
        date.set(Calendar.YEAR, (yearInput.getValue + "").toInt)
        date.set(Calendar.MONTH, months.indexOf((monthInput.getValue + "")))
        date.set(Calendar.DAY_OF_MONTH, (dayInput.getValue + "").toInt)

        system.setTime(date.getTimeInMillis)
        system.addBody(new Body(
          name = "Example body",
          mass = 2e30,
          radius = 2000,
          location = new geometry.Vector(0, 0, 0),
          velocity = new geometry.Vector(0, 0, 0),
          system = system))

        result = Some(system)
        dialog.setVisible(false)
      } catch {
        case e: IllegalArgumentException => scala.swing.Dialog.showMessage( null, e.getMessage.replace("requirement failed: ", ""), "Invalid input")
        case e:NumberFormatException => scala.swing.Dialog.showMessage( null, e.getMessage, "Invalid input")
      }
    }
  });

  def showNewSystemDialog(): Option[System] = {

    dialog.setVisible(true)
    return result

  }
}