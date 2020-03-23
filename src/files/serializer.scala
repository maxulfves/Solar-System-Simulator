package files

import java.io._
import system._
import scala.io.Source
import java.awt.Color

class Serializer()  {
  
  //TODO Add exceptions
  //TODO Checksum? 
  
  def deserialize(file:File):System = {
    val system = new System("")
    
    val reader = new BufferedReader(new FileReader(file))
    
    var line = reader.readLine()
    var checksum = Int.MinValue
    
    
    while(line != null && line.trim != "---"){
      println(line)
      val deter = line.take(2)
      val value = line.drop(2)
      
      deter match {
        case "N~" => system.setName(value)
        case "T~" => system.setTime(value.toLong)
        case "C~" => checksum = value.toInt //TODO checksum
        case _ => throw new IOException("Invalid line: " + line)
      }
      
      line = reader.readLine()
    }
    line = reader.readLine()
    while(line != null){
      
      println(line)
      line = line.trim
      if(line.trim.charAt(0) == '#'){
        val _type = line.trim().toLowerCase()
        
        var name = ""
        var mass = 0.0
        var location = new geometry.Vector(0,0,0)
        var velocity = new geometry.Vector(0,0,0)
        var radius = 0.0
        var color = Color.WHITE
        
        //Loop through every property of each object
        line = reader.readLine()
        while(line != null && line.charAt(0) != '#'){
          
          val deter = line.take(2)
          val value = line.drop(2)
          
          deter match {
            case "N|" => name = value.toString()
            case "M|" => mass = value.toDouble
            case "R|" => radius = value.toDouble
            case "L|" => location = {
              val loc = value.trim.split(",").map(_.toDouble)
              new geometry.Vector(loc(0), loc(1), loc(2))
            }
            case "V|" => velocity = {
              val loc = value.trim.split(",").map(_.toDouble)
              new geometry.Vector(loc(0), loc(1), loc(2))
            }
            case "C|" => {
            	val values = value.split(",").map(_.filter(_!= ' ').toInt )
              println(values.mkString(" "))
              color = new Color(values(0), values(1), values(2))
            }
            case ">|" => //Comments are ignored
            case _ => throw new IOException("Unrecognized input: "+ deter)
          }
          
          line = reader.readLine()
        }
        
        
        val body = _type match{
          case "#star" => new Star(name, mass, radius, location, velocity, system)
          case "#planet" => new Planet(name, mass, radius, location, velocity, system)
          case _ => throw new IOException("Invalid line: " + line )
          
        }
        body.setColor(color)
        system.addBody(body)
        
      }
      
      
    }
    
    system
  }
  
  def serialize(sys:System, file:File){
    val bw = new BufferedWriter(new FileWriter(file))
    
    bw.write("N~%s".format(sys.name))
    bw.newLine()
    bw.write("T~%s".format(sys.getTimeInMs))
    bw.newLine()
    bw.write("C~%s".format(0)) //TODO checksum
    bw.newLine()
    bw.write("---")
    bw.newLine()
    
    for(body:Body <- sys.bodies){
      
      val type_ = body match{
        case s:Star => "star"
        case s:Planet => "planet"
        case s:Satelite => //TODO
      }
      
      bw.write("#%s".format(type_))
      bw.newLine()
      
      bw.write("N|%s".format(body.getName))
      bw.newLine()
      
      bw.write("M|%s".format(body.getMass))
      bw.newLine()
      
      bw.write("R|%s".format(body.getRadius))
      bw.newLine()
      
      bw.write("L|%s, %s, %s".format(body.location.x, body.location.y, body.location.z))
      bw.newLine()
      
      bw.write("V|%s, %s, %s".format(body.velocity.x, body.velocity.y, body.velocity.z))
      bw.newLine()
      
      //bw.newLine()
      
      
    }
    
    bw.close()
    deserialize(file)
  }
  
}