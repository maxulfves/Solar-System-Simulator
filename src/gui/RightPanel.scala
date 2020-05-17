package gui
import scala.swing.TabbedPane
import scala.swing._
import system._
import gui.SimulatorApp._
import scala.swing.event._
import java.awt.Color
import geometry._
import scala.swing.GridBagPanel
import java.awt.GridBagConstraints
import javax.swing.DefaultComboBoxModel
import java.text.DecimalFormat
import javax.swing.SpinnerNumberModel
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent

/**
 * An object representing the right-side panel.
 * If the program were to be expanded further it should probably be subdivided into more files,
 * but for my intents and purposes it works fine as is.
 */
object RightPanel extends TabbedPane {

  minimumSize = new Dimension(300, winSize._2)
  background = Color.LIGHT_GRAY

  def updateSystem() {

    bodiesPanel.lw.listData = system.bodies.map(_.getName)
    graphPanel.update()
  }

  def disable(arg: Boolean) = {
    setDisable(arg, bodiesPanel)
  }

  private def setDisable(arg: Boolean, arg2: Container): Unit = {
    for (element <- arg2.contents) {
      element match {
        case e: TextField    => e.editable = arg
        case e: Button       => e.enabled = arg
        case e: CheckBox     => e.enabled = arg
        case e: GridBagPanel => setDisable(arg, e)
        case _               => //Do nothing
      }
    }
  }

  val bodiesPanel = new GridBagPanel {
    def updateSelectionData() = {

      val selection = system.bodies.filter(_.getName == lw.selection.items(0)).head
      tw_name.text = lw.selection.items(0)
      tw_mass.text = selection.getMass.toString()

      tw_x.text = selection.location.x + ""
      tw_y.text = selection.location.y + ""
      tw_z.text = selection.location.z + ""

      val col = selection.getColor()
      tw_colour.text = "#%02x%02x%02x".format(col.getRed, col.getGreen, col.getBlue)

      tw_vel_x.text = selection.velocity.x + ""
      tw_vel_y.text = selection.velocity.y + ""
      tw_vel_z.text = selection.velocity.z + ""

      labelSpeed.text = "Magnitude V: " + new DecimalFormat("00E0").format(selection.velocity.magnitude).replace("E", " * 10^") + " m/s"
      labelAcc.text = "Magnitude A: " + new DecimalFormat("00E0").format(selection.acceleration.magnitude).replace("E", " * 10^") + " m/s^2"
      radius.text = selection.getRadius + ""

    }
    val lw = new scala.swing.ListView(system.bodies.map(_.getName))
    val tw_name = new TextField()
    val tw_mass = new TextField()
    //Location

    val tw_x = new TextField
    val tw_y = new TextField
    val tw_z = new TextField
    //Velocity

    val tw_vel_x = new TextField
    val tw_vel_y = new TextField
    val tw_vel_z = new TextField

    val radius = new TextField

    val tw_colour = new TextField
    val labelSpeed = new Label("Magnitude V: ")
    val labelAcc = new Label("Magnitude A: ")

    def constraintsTop(x: Int, y: Int, width: Int = 1, height: Int = 1, weightx: Double = 0.0, weighty: Double = 0.0, fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None): Constraints = {
      val constraint = new Constraints
      constraint.anchor = GridBagPanel.Anchor.North
      constraint.gridx = x
      constraint.gridy = y
      constraint.gridwidth = width
      constraint.gridheight = height
      constraint.weightx = weightx
      constraint.weighty = weighty

      constraint.fill = fill
      constraint
    }

    val btn_save = new Button("Save") {
      reactions += {
        case action: ButtonClicked => {

          if (!(system.bodies - selectedBody).exists(_.getName == tw_name.text)) {
            selectedBody.setMass(tw_mass.text.toDouble)
            selectedBody.setName(tw_name.text)
            selectedBody.setLocation(new Vector(tw_x.text.toDouble, tw_y.text.toDouble, tw_z.text.toDouble))
            selectedBody.setVelocity(new Vector(tw_vel_x.text.toDouble, tw_vel_y.text.toDouble, tw_vel_z.text.toDouble))
            selectedBody.setColor(Color.decode(tw_colour.text))
            selectedBody.setRadius(radius.text.trim().toDouble)

            updateSystem()
          } else {
            Dialog.showMessage(null, tw_name.text + " already exists!", "Name taken")
          }
          println("updated")
        }
      }
    }

    val btn_remove = new Button("Remove") {
      reactions += {
        case action: ButtonClicked => {
          system.remove(selectedBody)
          updateSystem()

        }

      }
    }

    val btn_newBody = new Button("New body...") {
      reactions += {
        case action: ButtonClicked => {

          val name = Dialog.showInput(contents.head, "New label text", initial = "name")

          name match {
            case None     => //Cancel was pressed
            case Some("") => //Name is empty. Add temporary name
            case Some(a: String) => {
              val body = new Body(a, 0, 0, new Vector(0, 0, 0), new Vector(0, 0, 0), system)

              system.addBody(body)
              lw.listData = system.bodies.map(b => b.getName)

            }
            case _ => throw new IllegalArgumentException("This shouldn't happen")

          }

        }
      }
    }

    listenTo(lw.selection)

    reactions += {
      case SelectionChanged(`lw`) => {
        if (lw.selection.items.size != 0) {

          selectedBody = system.bodies.filter(_.getName == lw.selection.items(0)).head

          updateSelectionData()

        } else {
          if (lw.listData.size != 0) {
            lw.selectIndices(0)
          }
        }
      }
    }

    val bp = new GridBagPanel {
      def constraints(x: Int, y: Int, width: Int = 1, height: Int = 1, weightx: Double = 0.0, weighty: Double = 0.0, fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.Horizontal): Constraints = {
        val constraint = new Constraints
        constraint.gridx = x
        constraint.gridy = y
        constraint.gridwidth = width
        constraint.gridheight = height
        constraint.weightx = weightx
        constraint.weighty = weighty
        constraint.fill = fill
        constraint.anchor = GridBagPanel.Anchor.West

        constraint.ipadx = 10
        constraint
      }

      val weightLeft = 1
      val weightRight = 2

      add(new Label("Name"), constraints(0, 0, width = weightLeft))
      add(tw_name, constraints(1, 0, width = weightRight, fill = GridBagPanel.Fill.Horizontal))
      add(new Label("Mass"), constraints(0, 1, width = weightLeft))
      add(tw_mass, constraints(1, 1, weightx = 1, width = weightRight, fill = GridBagPanel.Fill.Horizontal))
      add(new Label("Radius"), constraints(0, 2, width = weightLeft))
      add(radius, constraints(1, 2, width = weightRight, fill = GridBagPanel.Fill.Horizontal))
      add(new swing.Separator, constraints(0, 3, width = (weightRight + weightLeft), fill = GridBagPanel.Fill.Horizontal))

      //Location
      add(new Label("Location X"), constraints(0, 4, width = weightLeft, fill = GridBagPanel.Fill.Horizontal))
      add(tw_x, constraints(1, 4, width = weightRight, fill = GridBagPanel.Fill.Horizontal))
      add(new Label("Location Y"), constraints(0, 5, width = weightLeft, fill = GridBagPanel.Fill.Horizontal))
      add(tw_y, constraints(1, 5, width = weightRight, fill = GridBagPanel.Fill.Horizontal))
      add(new Label("Location Z"), constraints(0, 6, width = weightLeft))
      add(tw_z, constraints(1, 6, width = weightRight, fill = GridBagPanel.Fill.Horizontal))
      add(new swing.Separator, constraints(0, 7, width = (weightRight + weightLeft), fill = GridBagPanel.Fill.Horizontal))

      //Velocity
      add(new Label("Velocity X"), constraints(0, 8, width = weightLeft))
      add(tw_vel_x, constraints(1, 8, width = weightRight, fill = GridBagPanel.Fill.Horizontal))
      add(new Label("Velocity Y"), constraints(0, 9, width = weightLeft))
      add(tw_vel_y, constraints(1, 9, width = weightRight, fill = GridBagPanel.Fill.Horizontal))
      add(new Label("Velocity Z"), constraints(0, 10, width = weightLeft))
      add(tw_vel_z, constraints(1, 10, width = weightRight, fill = GridBagPanel.Fill.Horizontal))

      add(labelSpeed, constraints(0, 11, width = (weightLeft + weightRight), fill = GridBagPanel.Fill.Horizontal, weightx = 1))
      add(new swing.Separator, constraints(0, 12, width = (weightRight + weightLeft), fill = GridBagPanel.Fill.Horizontal))

      add(labelAcc, constraints(0, 13, width = (weightLeft + weightRight), fill = GridBagPanel.Fill.Horizontal, weightx = 1))
      add(new swing.Separator, constraints(0, 14, width = (weightRight + weightLeft), fill = GridBagPanel.Fill.Horizontal))

      //Control buttons
      add(new Label("Colour"), constraints(0, 15, width = weightLeft, fill = GridBagPanel.Fill.Horizontal))
      add(tw_colour, constraints(1, 15, width = weightRight, fill = GridBagPanel.Fill.Horizontal))

      add(btn_save, constraints(0, 16, width = 1, fill = GridBagPanel.Fill.Horizontal))
      add(btn_remove, constraints(1, 16, width = 1, fill = GridBagPanel.Fill.Horizontal))
      add(btn_newBody, constraints(2, 16, width = 1, fill = GridBagPanel.Fill.Horizontal))

    }
    lw.selectIndices(0)

    add(lw, constraintsTop(0, 1, weightx = 1, fill = GridBagPanel.Fill.Both))
    add(bp, constraintsTop(0, 2, fill = GridBagPanel.Fill.Both))

    add(new scala.swing.Separator, constraintsTop(0, 3, 1, 1, 0, 1, GridBagPanel.Fill.Horizontal))

  }


  val cameraPanel = new GridBagPanel() {

    override def paint(g: Graphics2D) = {
      super.paint(g)

      val arg = gui.SimulatorApp.getIsPaused

      if (!arg) {
        rotX.text = camera.getRotX.toDegrees.toString()
        rotY.text = camera.getRotY.toDegrees.toString()
        rotZ.text = camera.getRotZ.toDegrees.toString()
        angle_txt.text = camera.getFocalLength.toString()
        dis.text = camera.getDistance + ""

      }
      for (element <- contents) {
        element match {
          case e: TextField    => e.editable = arg
          case e: Button       => e.enabled = arg
          case e: CheckBox     => e.enabled = arg
          case e: GridBagPanel => setDisable(arg, e)
          case _               => //Do nothing
        }
      }

      repaint
    }

    val rotX = new TextField(camera.getRotX + "")
    val rotY = new TextField(camera.getRotY + "")
    val rotZ = new TextField(camera.getRotZ + "")
    val angle_txt = new TextField(camera.getFocalLength + "")

    val dis = new TextField(camera.getDistance + "")

    def constraints(x: Int, y: Int, width: Int = 1, height: Int = 1, weightx: Double = 0.0, weighty: Double = 0.0): Constraints = {
      val constraint = new Constraints
      constraint.gridx = x
      constraint.gridy = y
      constraint.gridwidth = width
      constraint.gridheight = height
      constraint.weightx = weightx
      constraint.weighty = weighty
      constraint.fill = GridBagPanel.Fill.Horizontal
      constraint.anchor = GridBagPanel.Anchor.NorthWest

      constraint.ipadx = 10
      constraint
    }

    var rowCount = 0
    def AddRow(a: Label, b: TextField): Unit = {
      add(a, constraints(0, rowCount, width = 2))
      a.horizontalAlignment = Alignment.Left
      add(b, constraints(2, rowCount, width = 2, weightx = 1))
      rowCount += 1
    }

    AddRow(new Label("Distance: "), dis)
    AddRow(new Label("Angle: "), angle_txt)
    AddRow(new Label("Rotation X: "), rotX)
    AddRow(new Label("Rotation Y: "), rotY)
    AddRow(new Label("Rotation Z: "), rotZ)

    add(new Button("Update") {
      reactions += {
        case action: ButtonClicked => {

          try {
            camera.rotateTo(
              rotX.text.toDouble.toRadians,
              rotY.text.toDouble.toRadians,
              rotZ.text.toDouble.toRadians)

            camera.setFocalLength(angle_txt.text.toDouble)
            camera.setDistance(dis.text.toDouble)
          } catch {
            case e: NumberFormatException => Dialog.showMessage(message = e.getMessage, title = "Invalid input")
          }
        }
      }
    }, constraints(0, rowCount, width = 4, weighty = 1))

    //add(new scala.swing.Separator, constraints(0, 6, 4, 1, 0, 1, GridBagPanel.Fill.Horizontal))

  }

  

  val graphPanel = new GridBagPanel {
    val fields = Seq("", "X", "Y", "Z", "Velocity X", "Velocity Y", "Velocity Z")

    val bodyData = new ComboBox(Seq(""))
    val bodyDataB = new ComboBox(Seq(""))

    override def paint(g: Graphics2D) {
      super.paint(g)
      fieldDataB.enabled = !dataY.selected
      bodyDataB.enabled = !dataY.selected || fieldData.selection.item == "Distance to..."
      btn_track.enabled = SimulatorApp.graphReady
      btn_clear.enabled = graph.length != 0
    }

    def update() {
      val model = new DefaultComboBoxModel("" +: system.bodies.map(_.name).toArray)
      val modelB = new DefaultComboBoxModel("" +: system.bodies.map(_.name).toArray)

      val prevA = bodyData.selection.item
      val prevB = bodyDataB.selection.item

      bodyData.peer.setModel(model)
      bodyDataB.peer.setModel(modelB)

      bodyData.selection.item = prevA
      bodyDataB.selection.item = prevB
      /*
      if(indA <= bodyData.peer.getItemCount && indB <= bodyDataB.peer.getItemCount){

        bodyDataB.selection.index = indB
      }*/

    }

    val restartButton = new Button("Restart") {
      reactions += {
        case clickEvent: ButtonClicked => {
          restart
        }
      }
    }

    val btn_clear = new Button("Clear") {
      reactions += {
        case clock: ButtonClicked => graph.clear
      }
    }

    val btn_track: Button = new Button("Track") {
      override def paint(g: Graphics2D) {
        super.paint(g)
        this.text = gui.SimulatorApp.tracking match {
          case true  => "Stop tracking"
          case false => "Track"
        }
      }

      reactions += {
        case click: ButtonClicked =>
          {
            gui.SimulatorApp.tracking = !gui.SimulatorApp.tracking
          }

      }
    }

    val dataY = new ToggleButton {
      reactions += {
        case click: ButtonClicked => {
          graph.clear
          SimulatorApp.tracking = false
          graphData.xTime = this.selected
          println(graphData.xTime)
        }
      }
    }
    dataY.selected = true
    dataY.text = "Time on X-axis?"

    val fieldData = new ComboBox(fields :+ "Distance to...")
    val fieldDataB = new ComboBox(fields)

    listenTo(bodyData.selection)
    listenTo(bodyDataB.selection)
    listenTo(fieldData.selection)
    listenTo(fieldDataB.selection)

    //When a field is changed, update the graphData object.
    reactions += {
      case SelectionChanged(`bodyData`) => {
        if (bodyDataB.selection.item != "") {
          graphData.bodyA = system.bodies.find(p => p.getName == bodyDataB.selection.item)
        } else {
          graphData.bodyA = None
        }
        graphData.bodyA = system.bodies.find(p => p.getName == bodyData.selection.item)
        SimulatorApp.tracking = false
        graph.clear
      }
      case SelectionChanged(`bodyDataB`) => {
        if (bodyDataB.selection.item != "") {
          graphData.bodyB = system.bodies.find(p => p.getName == bodyDataB.selection.item)
        } else {
          graphData.bodyB = None
        }

        graph.clear
        SimulatorApp.tracking = false
      }
      case SelectionChanged(`fieldData`) => {
        if (fieldData.selection.item != "") {
          graphData.field = Some(fieldData.selection.item)
        } else {
          graphData.field = None
        }

        graph.clear
        SimulatorApp.tracking = false
      }
      case SelectionChanged(`fieldDataB`) => {
        if (fieldDataB.selection.item != "") {
          graphData.fieldB = Some(fieldDataB.selection.item)
        } else {
          graphData.fieldB = None
        }

        graph.clear
        SimulatorApp.tracking = false
      }

      case SelectionChanged(_) => //Naught
    }

    def constraints(x: Int, y: Int, width: Int = 1, height: Int = 1, weightx: Double = 0.0, weighty: Double = 0.0): Constraints = {
      val constraint = new Constraints
      constraint.gridx = x
      constraint.gridy = y
      constraint.gridwidth = width
      constraint.gridheight = height
      constraint.weightx = weightx
      constraint.weighty = weighty
      constraint.fill = GridBagPanel.Fill.Both
      constraint.anchor = GridBagPanel.Anchor.NorthWest

      constraint.ipadx = 10
      constraint
    }

    var rowCount = 0
    def addRow(a: String, b: Component) {
      val lab = new Label(a)
      lab.horizontalAlignment = Alignment.Left
      add(lab, constraints(x = 0, y = rowCount, width = 1, weightx = 0))
      add(b, constraints(x = 1, y = rowCount, width = 1, weightx = 1))

      rowCount += 1
    }
    def addRow(s: String) {
      val lab = new Label(s)
      add(lab, constraints(x = 0, y = rowCount, width = 1, weightx = 0))
      lab.horizontalAlignment = Alignment.Left
      rowCount += 1
    }

    def addRow(comp: Component) {
      add(comp, constraints(x = 0, y = rowCount, width = 2, weightx = 0))
      rowCount += 1
    }

    addRow("Y-axis")
    addRow("Body", bodyData)
    addRow("Field", fieldData)

    addRow("Type of track: ", dataY)

    addRow("X-axis")
    addRow("Body", bodyDataB)
    addRow("Field", fieldDataB)

    rowCount += 1

    addRow(btn_clear)
    addRow(btn_track)

    add(graph, constraints(x = 0, y = rowCount, width = 2, height = 3, weightx = 0, weighty = 1))

    for (i <- contents) {
      val dim = i.preferredSize
      dim.width = i.maximumSize.width
      dim.height = i.preferredSize.height + 3
      i.maximumSize = dim

    }
  }

  val simulationPanel = new GridBagPanel() {

    val l_name = new Label("System name: ")
    val time = new Label("Time: ")
    val endTime = new Label("End time: ")
    val totalMass = new Label("Total mass: ")
    val bodyCount = new Label("Number of bodies: ")
    val timestep = new Label("Timestep: ")

    override def paint(g: Graphics2D) {
      super.paint(g)
      l_name.text = "System name: " + system.getName
      time.text = "Time: " + system.getTime
      endTime.text = "End time: " + system.getEnd
      totalMass.text = "Total mass: " + system.totalMass
      bodyCount.text = "Body NO: " + system.bodies.length
      timestep.text = "Timestep:" + system.getStepSize
      repaint()
      revalidate()
    }

    def constraints(x: Int, y: Int, width: Int = 1, height: Int = 1, weightx: Double = 0.0, weighty: Double = 0.0): Constraints = {
      val constraint = new Constraints
      constraint.gridx = x
      constraint.gridy = y
      constraint.gridwidth = width
      constraint.gridheight = height
      constraint.weightx = weightx
      constraint.weighty = weighty
      constraint.fill = GridBagPanel.Fill.Horizontal
      constraint.anchor = GridBagPanel.Anchor.NorthWest

      constraint.ipadx = 10
      constraint.ipady = 5
      constraint
    }

    var rowCount = 0
    def addRow(c: Label) = {
      c.horizontalAlignment = Alignment.Left
      add(c, constraints(x = 0, y = rowCount, width = 1, height = 1, weightx = 1, weighty = 0))

      rowCount += 1
    }

    def addRow(c: String) = {
      val n = new Label(c)
      n.horizontalAlignment = Alignment.Left
      add(n, constraints(x = 0, y = rowCount, width = 1, height = 1, weightx = 1, weighty = 0))
      rowCount += 1
    }

    val fps_spinner = new javax.swing.JSpinner(new SpinnerNumberModel(simulationCanvasPanel.fps, 1, 200, 1))
    val sps_spinner = new javax.swing.JSpinner(new SpinnerNumberModel(simulationCanvasPanel.sps, 1, 1000, 1))

    fps_spinner.addChangeListener(new ChangeListener() {
      override def stateChanged(e: ChangeEvent) {
        simulationCanvasPanel.fps = (fps_spinner.getValue + "").toInt
      }
    })
    
    sps_spinner.addChangeListener(new ChangeListener() {
      override def stateChanged(e: ChangeEvent) {
        simulationCanvasPanel.sps = (sps_spinner.getValue + "").toInt
      }
    })
    
    addRow(l_name)
    addRow(time)
    addRow(endTime)
    addRow(timestep)
    addRow(totalMass)
    addRow(bodyCount)
    
    add(new Separator, constraints(x = 0, y = rowCount, width = 1, height = 1, weightx = 0, weighty = 0))
    rowCount += 1
    addRow("Attempted frames per second [1, 200]")
    add(Component.wrap(fps_spinner), constraints(x = 0, y = rowCount, width = 1, height = 1, weightx = 0, weighty = 0))
    rowCount += 1
    add(new Separator, constraints(x = 0, y = rowCount, width = 1, height = 1, weightx = 0, weighty = 0))
    rowCount += 1
    addRow("Attempted simulations per second [1, 1000]")
    add(Component.wrap(sps_spinner), constraints(x = 0, y = rowCount, width = 1, height = 1, weightx = 0, weighty = 1))
    rowCount += 1
  }
  
  pages.addOne(new TabbedPane.Page("Bodies", bodiesPanel))
  pages.addOne(new TabbedPane.Page("Camera", cameraPanel))
  pages.addOne(new TabbedPane.Page("Graph", graphPanel))
  pages.addOne(new TabbedPane.Page("Simulation", simulationPanel))

}