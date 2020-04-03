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

object SimulatorApp extends SimpleSwingApplication {
	var openedFile = new File( Constants.exampleSource )
	var isExample = true

	val winSize = ( 1000, 900 )

	val day = geometry.Constants.dt

	val mySerializer = new files.Serializer()
	var system = mySerializer.deserialize( openedFile )
	val initial = system.copy

	val fps = Constants.fps
	//TODO make nicer
	val d: Double = 1.5e11 //m
	val f: Double = 2.0 //m

	val plane = new Plane( -math.sqrt( 2 ), 0, -math.sqrt( 2 ), d * 2 )

	val camera = new Camera(new Plane( 0, 0, 1.0, d ),
		new geometry.Point( 0, 0, -( d + f ) ),
		new geometry.Vector( 0, 1, 0 ) )

	val angle = ( math.Pi * 2.0 ) / 1000

	def onKeyPress( keyCode: Value ) = keyCode match {
		case Key.Plus => camera.zoomIn
		case Key.Minus => camera.zoomOut
		case Key.Up => camera.rotateBy( angle, 0, 0 )
		case Key.Down => camera.rotateBy( -angle, 0, 0 )
		case Key.Left => camera.rotateBy( 0, angle, 0 )
		case Key.Right => camera.rotateBy( 0, -angle, 0 )
		case Key.Period => camera.rotateBy( 0, 0, angle )
		case Key.Comma => camera.rotateBy( 0, 0, -angle )
		case Key.Space => togglePause
		case Key.R => restart
		case _ => // do nothing
	}

	camera.rotateBy( 0, 0, 0 )

	//Test
	val pauseButton = new Button( "Pause" )
	val restartButton = new Button( "Restart" ) {
		reactions += {
			case clickEvent: ButtonClicked => {
				restart
			}
		}
	}
	
	/**
	 * Adds data to the graph in Graph-page.
	 */
	def addGraphData(){
		var x = 0.0
		var y = 0.0
		
		val dat = system.bodies(1).location.x
		
		
		var distanceTo = false
		
		graphData.bodyA match {
			case None => 
			case Some(a) => {
				graphData.field match {
					case Some(fieldA) => {
						fieldA match {
							case "X" => y = a.location.x
							case "Y" => y = a.location.y
							case "Z" => y = a.location.z
							case "Velocity X" => y = a.velocity.x
							case "Velocity Y" => y = a.velocity.y
							case "Velocity Z" => y = a.velocity.z
							case "Distance to..." => distanceTo = true
							
						}
					}
					case None => //Ignore
				}
			}
		}
		
		graphData.bodyB match {
			case None => 
			case Some(a) => {
				graphData.fieldB match {
					case Some(fieldB) => {
						fieldB match {
							case "X" => x = a.location.x
							case "Y" => x = a.location.y
							case "Z" => x = a.location.z
							case "Velocity X" => x = a.velocity.x
							case "Velocity Y" => x = a.velocity.y
							case "Velocity Z" => x = a.velocity.z
						}
					}
					case None => //Ignore
				}
			}
		}
		
		if(distanceTo && (graphData.bodyA.isDefined && graphData.bodyB.isDefined)){
			y = graphData.bodyA.head.location.toPoint().distanceTo(graphData.bodyB.head.location.toPoint())
		}
		
		if(graphData.xTime){
			graph.addData(time, y)
		}else{
			graph.addData(x, y)		
		}
		
	}
	
	object graphData {
		var bodyA:Option[Body] = None
		var bodyB:Option[Body] = None
		var field:Option[String] = None
		var fieldB:Option[String] = None
		var xTime:Boolean = true
	}
	
	
	
	val prompt = new Label( "Date: " )
	prompt.size.setSize( prompt.size.width * 2, prompt.size.height )

	val timeController = new BoxPanel( Orientation.Horizontal )
	timeController.contents += restartButton
	timeController.contents += pauseButton
	timeController.contents += prompt

	val topPanel = new scala.swing.MenuBar {
		contents += new Menu( "File" ) {
			contents += new MenuItem( "New..." ) {
				reactions += {
					case clickEvent: ButtonClicked => {
						openNewSystemDialog
					}
				}
			}

			contents += new MenuItem( "Load..." ) {
				reactions += {
					case clickEvent: ButtonClicked => load
				}
			}

			contents += new MenuItem( "Save" ) {
				reactions += {
					case clickEvent: ButtonClicked => {
						if ( !isExample ) {
							save
						} else {
							//TODO alert
							Dialog.showMessage( this, "You shouldn't overwrite an example file! \nTry \"Save as...\" instead. ", "Warning!", Dialog.Message.Warning )
						}
					}
				}
			}

			contents += new MenuItem( "Save as..." ) {
				reactions += {
					case clickEvent: ButtonClicked => saveAs
				}
			}

			contents += new MenuItem( "Exit" ) {
				reactions += {
					case clickEvent: ButtonClicked => exit
				}
			}

		}
		contents += new Menu( "Simulation" )

		contents += new Menu( "Display" ) {
			contents += new MenuItem( "Properties..." )
			contents += new MenuItem( "Camera settings... " )
			contents += new MenuItem( "Toggle vectors" )
		}
		contents += new Menu( "Window" ) {
			contents += new MenuItem( "Show view..." )
		}

		contents += new Menu( "Help" ) {
			contents += new MenuItem( "Instructions..." )
			contents += new MenuItem( "About Solar System" )
			contents += new MenuItem( "test" )
		}

	}

	
	
	var selectedBody: Body = system.bodies( 0 )
	def updateSelectionData() = {
		val selection = system.bodies.filter( _.getName == lw.selection.items( 0 ) ).head
		tw_name.text = lw.selection.items( 0 )
		tw_mass.text = selection.getMass.toString()

		tw_x.text = selection.location.x + ""
		tw_y.text = selection.location.y + ""
		tw_z.text = selection.location.z + ""

		tw_vel_x.text = selection.velocity.x + ""
		tw_vel_y.text = selection.velocity.y + ""
		tw_vel_z.text = selection.velocity.z + ""

		toggleGhost.selected = selection.getIsGhost
	}
	
	
	

	val newSystem: Dialog = new Dialog() {
		title = "Create a new simulation"
		//preferredSize = new Dimension(400, 400)

		contents = new scala.swing.BoxPanel( Orientation.Vertical ) {

			contents += new scala.swing.Label( "Name" )

			val nameInput = new scala.swing.TextField
			val yearDropdown = new scala.swing.ComboBox( Seq.tabulate( 2050 )( _ + 1 ).drop( 1969 ).map( _.toString() ) )
			val monthDropdown = new scala.swing.ComboBox( Seq.tabulate( 12 )( _ + 1 ).drop( 0 ) )
			val dayDropdown = new scala.swing.ComboBox( Seq.tabulate( 31 )( _ + 1 ).drop( 0 ) )

			val createNewButton = new Button( "Create!" ) {
				reactions += {
					case action: ButtonClicked => {
						val createdSystem = new System( nameInput.text )
						//TODO Is name legit?
						val date = Calendar.getInstance

						println( yearDropdown.selection.item + "" )
						date.set( Calendar.YEAR, yearDropdown.selection.item.toInt )
						date.set( Calendar.MONTH, monthDropdown.selection.item.toInt )
						date.set( Calendar.DAY_OF_MONTH, dayDropdown.selection.item.toInt )

						createdSystem.setTime( date.getTimeInMillis )

						system = createdSystem

						newSystem.close()
						println( "created!" )
					}

				}
			}
			nameInput.maximumSize = new Dimension( 200, 20 )
			contents += nameInput
			contents += new scala.swing.Label( "Start year" )
			contents += yearDropdown
			contents += new scala.swing.Label( "Start month" )
			contents += monthDropdown
			contents += new scala.swing.Label( "Start date" )
			contents += dayDropdown
			contents += createNewButton

		}
	}

	def openNewSystemDialog = {
		newSystem.open()
		newSystem.background = Color.RED
		newSystem.centerOnScreen()
		newSystem.visible = true
	}

	def restart = {
		println( "restarted" )
		system.set( initial.copy )
		lw.listData = system.bodies.map( b => b.getName )
	}

	def exit = {
		//TODO Add grace.
		System.exit( 1 )
	}

	def save = mySerializer.serialize( system, openedFile )

	def load = {

		val fileDialog = new scala.swing.FileChooser( new File( Constants.simulationFileDirectory ) )
		fileDialog.title = "Load a system"
		fileDialog.fileFilter = new FileNameExtensionFilter( "Solarsystem state (.%s)".format( Constants.extension ), Constants.extension )
		fileDialog.multiSelectionEnabled = false
		fileDialog.controlButtonsAreShown = true
		fileDialog.fileHidingEnabled = false

		val selection = fileDialog.showOpenDialog( mainPanel );
		if ( selection == FileChooser.Result.Approve ) {
			val fileToLoad: File = fileDialog.selectedFile
			println( "Load file: " + fileToLoad.getAbsolutePath() );

			isExample = fileToLoad.getParent.equals( new File( Constants.exampleDirectory ).getAbsolutePath )

			println( isExample )

			system = ( mySerializer.deserialize( fileToLoad ) )

			//Set the listview to current system.
			lw.listData = system.bodies.map( _.getName )

			initial.set( system.copy )
			openedFile = fileToLoad

			//TODO Alert if operation failed.
			//TODO Check if file is legit
			//TODO
		}

	}

	def saveAs = {
		val fileDialog = new scala.swing.FileChooser( new File( Constants.simulationFileDirectory ) )

		fileDialog.title = "Save as..."
		fileDialog.fileFilter = new FileNameExtensionFilter( "Solarsystem state (.%s)".format( Constants.extension ), Constants.extension )
		fileDialog.multiSelectionEnabled = false
		fileDialog.controlButtonsAreShown = true
		fileDialog.fileHidingEnabled = false

		val defaultName = system.name + "_" + system.getDate.replace( ' ', '_' ) + "." + Constants.extension

		fileDialog.selectedFile = new File( defaultName )

		val selection = fileDialog.showSaveDialog( mainPanel );

		if ( selection == FileChooser.Result.Approve ) {
			val fileToSave: File = fileDialog.selectedFile
			println( "Save as file: " + fileToSave.getAbsolutePath() );
			mySerializer.serialize( system, fileToSave )
			//TODO Alert if operation failed.
		}
	}

	val rightpanel = new scala.swing.TabbedPane() {
		preferredSize = new Dimension( 300, winSize._2 )
		background = Color.LIGHT_GRAY
	}

	val tw_name = new TextField()
	val tw_mass = new TextField()
	//Location
	val lb_x = new Label( "Location X" )
	val lb_y = new Label( "Location Y" )
	val lb_z = new Label( "Location Z" )

	val tw_x = new TextField
	val tw_y = new TextField
	val tw_z = new TextField

	//Velocity
	val lb_vel_x = new Label( "Velocity X" )
	val lb_vel_y = new Label( "Velocity Y" )
	val lb_vel_z = new Label( "Velocity Z" )

	val tw_vel_x = new TextField
	val tw_vel_y = new TextField
	val tw_vel_z = new TextField
	val toggleGhost = new scala.swing.CheckBox
	toggleGhost.text = "Is ghost? "
	

	val lw = new scala.swing.ListView( system.bodies.map( _.getName ) )
	
	val bodies_page = new TabbedPane.Page( "Bodies", new BoxPanel( Orientation.Vertical ) {

		//Control buttons
		
		val btn_save = new Button( "Save" ) {
			reactions += {
				case action: ButtonClicked => {
					//TODO What happens when one saves?
					selectedBody.setMass( tw_mass.text.toDouble )
					selectedBody.setGhost( toggleGhost.selected )
					
					//TODO Is name taken? Alt. Is name legit?
					selectedBody.setName( tw_name.text )

					selectedBody.setLocation( new Vector( tw_x.text.toDouble, tw_y.text.toDouble, tw_z.text.toDouble ) )
					selectedBody.setVelocity( new Vector( tw_vel_x.text.toDouble, tw_vel_y.text.toDouble, tw_vel_z.text.toDouble ) )

					println( "updated" )
				}

			}
		}
		val btn_remove = new Button( "Remove" ) {
			reactions += {
				case action: ButtonClicked => {

					system.remove( selectedBody )
					lw.listData = system.bodies.map( _.getName )

				}

			}
		}

		val btn_newBody = new Button( "New body..." ) {
			reactions += {
				case action: ButtonClicked => {

					val name = Dialog.showInput( contents.head, "New label text", initial = "name" )

					name match {
						case None => //Cancel was pressed
						case Some( "" ) => //Name is empty. Add temporary name
						case Some( a: String ) => {
							val body = new Planet( a, 0, 0, new Vector( 0, 0, 0 ), new Vector( 0, 0, 0 ), system )
							body.setGhost( true )

							system.addBody( body )
							lw.listData = system.bodies.map( b => b.getName )

						}
						case _ => throw new IllegalArgumentException( "This shouldn't happen" )

					}

				}
			}
		}
		

		listenTo( lw.selection )

		reactions += {
			case SelectionChanged( `lw` ) => {
				if ( lw.selection.items.size != 0 ) {

					selectedBody = system.bodies.filter( _.getName == lw.selection.items( 0 ) ).head

					updateSelectionData()

				} else {
					if ( lw.listData.size != 0 ) {
						lw.selectIndices( 0 )
					}
				}
			}
		}
		
		val bp = new BoxPanel( Orientation.Vertical ) {
			contents += new Label( "Name" )
			contents += tw_name
			contents += new Label( "Mass" )
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
			contents += toggleGhost
			contents += btn_save
			contents += btn_remove
			contents += btn_newBody

			for ( i <- contents ) {
				val dim = i.preferredSize
				dim.width = i.maximumSize.width
				i.maximumSize = dim

			}

		}
		lw.selectIndices( 0 )
		lw.fixedCellWidth = 300

		contents += lw
		contents += bp

	} )
	
	
	rightpanel.pages.addOne( bodies_page)

	rightpanel.pages.addOne( new TabbedPane.Page( "Camera settings", new ScrollPane() {
		val rotX = new TextField( camera.rotationX + "" )
		val rotY = new TextField( camera.rotationY + "" )
		val rotZ = new TextField( camera.rotationZ + "" )
		val fLen = new TextField( camera.fLen2 + "" )
		val dis = new TextField( camera.plane.D + "" )

		contents = new BoxPanel( Orientation.Vertical ) {
			contents += new Label( "Distance: " )
			contents += dis

			contents += new Label( "Focal length" )
			contents += fLen

			contents += new Label( "Rotation X: " )
			contents += rotX

			contents += new Label( "Rotation Y: " )
			contents += rotY

			contents += new Label( "Rotation Z: " )
			contents += rotZ

			contents += new Button( "Update" ) {
				reactions += {
					case action: ButtonClicked => {
						camera.rotateTo(rotX.text.toDouble / 360 * ( math.Pi * 2 ),
							rotY.text.toDouble / 360 * ( math.Pi * 2 ),
							rotZ.text.toDouble / 360 * ( math.Pi * 2 ) )

						camera.setFocalLength( fLen.text.toDouble )
						camera.setDistance( dis.text.toDouble )

					}
				}
			}

			for ( i <- contents ) {
				val dim = i.preferredSize
				dim.width = i.maximumSize.width
				i.maximumSize = dim

			}

		}
	} ) )
	
	
	val graph = new Graph()
	rightpanel.pages.addOne( new TabbedPane.Page( "Graph", new BoxPanel( Orientation.Vertical ){
		val fields = Seq("X", "Y", "Z", "Velocity X", "Velocity Y", "Velocity Z")
		val bodies = system.bodies.map(_.name).toSeq
		
		
		val restartButton = new Button( "Restart" ) {
			reactions += {
				case clickEvent: ButtonClicked => {
					restart
				}
			}
		}
		
		val btn_clear = new Button("Clear") {
			reactions += {
				case clock:ButtonClicked => graph.clear
			}
		}
		
		val dataY = new ToggleButton {
			reactions += {
				case click: ButtonClicked => {
					graphData.xTime = this.selected
					println(graphData.xTime)
				}
			}
		}
		
		dataY.text = "Time on X-axis?"
		
		val bodyData = new ComboBox(bodies)
		val bodyDataB = new ComboBox(bodies)
		val fieldData = new ComboBox(fields :+ "Distance to...")
		val fieldDataB = new ComboBox(fields)
		
		
		listenTo( bodyData.selection )
		listenTo( bodyDataB.selection )
		listenTo( fieldData.selection )
		listenTo( fieldDataB.selection )

		//When a field is changed, update the graphData object. 
		reactions += {
			case SelectionChanged( `bodyData` ) => {
				graphData.bodyA = system.bodies.find(p => p.getName == bodyData.selection.item)
				
			}
			case SelectionChanged( `bodyDataB` ) => {
				graphData.bodyB = system.bodies.find(p => p.getName == bodyDataB.selection.item)
			}
			case SelectionChanged( `fieldData` ) => {
				graphData.field = Some(fieldData.selection.item)
			}
			case SelectionChanged( `fieldDataB` ) => {
				graphData.fieldB = Some(fieldDataB.selection.item)
			}
			
			case SelectionChanged(_) => //Naught
		}
		
		contents += new Label("Y-axis")
		contents += new Label("Body")
		contents += bodyData
		contents += new Label("Field")
		contents += fieldData
		
		
		contents += new Label("Type of track ")
		
		contents += new Separator
		contents += new Label("X-axis")
		
		contents += dataY
		
		contents += new Label("   ")
		contents += new Separator
		
		contents += new Label("Other body ")
		contents += bodyDataB
		
		contents += new Label("Field")
		contents += fieldDataB
		
		contents += graph
		contents += btn_clear
		
		for ( i <- contents ) {
			val dim = i.preferredSize
			dim.width = i.maximumSize.width 
			dim.height = i.preferredSize.height + 3
			i.maximumSize = dim 

		}
		
	}))

	val midPanel = new FlowPanel()

	midPanel.contents += mainPanel
	midPanel.contents += rightpanel

	val wholePanel = new BoxPanel( Orientation.Vertical )
	wholePanel.contents += midPanel
	wholePanel.contents += timeController

	val allContent = new MainFrame {
		contents = wholePanel
		menuBar = topPanel
		title = "Solar system simulator"
	}

	def top = this.allContent

	this.listenTo( pauseButton )

	pauseButton.reactions += {
		case clickEvent: ButtonClicked => togglePause
	}

	var x = 0

	private var isPaused = false

	def togglePause = {
		isPaused = !isPaused
		
		//TODO Grey out the inputs while isPaused == false. Alternatively keep it as it is. 
		
		pauseButton.text = isPaused match {
			case true => "Play"
			case false => "Pause"
		}
	}

	def mainPanel = new Panel {
		preferredSize = new Dimension( winSize._1, winSize._2 )
		focusable = true
		listenTo( keys )
		reactions += {
			case KeyPressed( _, key, _, _ ) =>
				onKeyPress( key )
		}

		override def paint( g: Graphics2D ) {
			//printOutput()

			if ( !isPaused ) {
				system.update
			}

			val img: BufferedImage = camera.capture( system )

			g.drawImage( img, null, 0, 0 )
			g.setColor( Color.GREEN )

			val output = system.getDate
			val rotX = "X: " + camera.rotationX / ( 2 * math.Pi ) * 360
			val rotY = "Y: " + camera.rotationY / ( 2 * math.Pi ) * 360
			val rotZ = "Z: " + camera.rotationZ / ( 2 * math.Pi ) * 360

			val font = new Font( "Bob", 12, 20 )
			g.setFont( font )
			val back = g.getFontMetrics( font ).stringWidth( output )
			val txtHeight = g.getFontMetrics( font ).getHeight

			/*
			g.drawString( output, winSize._1 - back - 8, winSize._2 - 10 )

			g.drawString( camera.fLen2 + "f", winSize._1 - back - 8, winSize._2 - txtHeight * 6 )
			g.drawString( rotX, winSize._1 - back - 8, winSize._2 - txtHeight * 5 )
			g.drawString( rotY, winSize._1 - back - 8, winSize._2 - txtHeight * 4 )
			g.drawString( rotZ, winSize._1 - back - 8, winSize._2 - txtHeight * 3 )
			*/
			prompt.text = output

			time += 1

			Thread.sleep( ( 1000.0 / fps ).toInt )
			repaint()
			revalidate()

			if ( rightpanel.selection.page.title == "Bodies" ) {
				if( !isPaused ){
					updateSelectionData()
				}
			}
			
			addGraphData()
			
		}


	}

	var i = 0
	def printOutput() {
		i += 1
	}
	var time = 0
}
  
