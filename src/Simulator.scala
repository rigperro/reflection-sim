
import scala.swing._
import scala.swing.event._
import javax.swing.UIManager
import java.awt.{Graphics2D,Color,Font,BasicStroke}
import java.awt.geom._
import scala.math._
import java.io._
import scala.io.Source

object Simulator extends SimpleSwingApplication {
 
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  
  val objectsText = Seq[String]("Mirror, straight (white)", "Mirror, concave (red)",  "Lens, concave (green)", "Lens, convex (blue)")
  val objectsMap = scala.collection.mutable.Map[(Int, Int, Int, Int), Reflective]()
  val lightsMap = scala.collection.mutable.Map[(Int, Int), Light]()
  val shines = scala.collection.mutable.Map[(Int, Int), Shine]()
  def addShine(x: (Int,Int)) = shines += x -> new Shine
  var rolls = 0
  def objIndex = rolls % 4 //gives the current chosen object for some methods
  
  var angle = 0.0
  
  var automated = false
  
  def updInfo = { //updates status text
    if (lighting == 1) info.text = "Placing lightsource at angle " + angle.toInt + "°." else if (lighting == 2) info.text = "Click anywhere to start simulation." else
    info.text = "Placing object of type: " + objectsText(objIndex) + " at angle " + angle.toInt + "°."
  }
  
  var lighting = 0 //stores the state of the simulation in progress

  
  val colors = Seq[Color](Color.white, Color.red, Color.green, Color.blue)
  def getColor(obj: Reflective): Color = {
      if (obj.isInstanceOf[StraightMirror]) return Color.white
      if (obj.isInstanceOf[ConcaveMirror]) return Color.red
      if (obj.isInstanceOf[ConcaveLens]) return Color.green
      if (obj.isInstanceOf[ConvexLens]) return Color.blue
      else Color.black
  }
  val info = new Label
  updInfo

  
  def top = new MainFrame {
    this.menuBar = new MenuBar {
      contents += new Menu("Start") {
        contents += new MenuItem(Action("Help"){help})
        contents += new MenuItem(Action("Add random objects"){randomize})
        contents += new MenuItem(Action("Preset scenario: Box"){preset})
        contents += new MenuItem(Action("Clear everything"){clear})
        contents += new MenuItem(Action("Save state"){save})
        contents += new MenuItem(Action("Load state"){load})
        contents += new Separator
        contents += new MenuItem(Action("Quit") {dispose()})
      }
    }
    var mouseX = 0
    var mouseY = 0
    var mouseclicked = false
    
    title = "Reflections"
    preferredSize = new Dimension(1000,800)
    val canvas = new Panel {
  opaque = true
  background = Color.black
  override def paintComponent(g: java.awt.Graphics2D) {
    super.paintComponent(g);
    if (mouseclicked) {
      lighting match {
       
        case 1 => { //When L is pressed, variable lighting is set to 1, thus placing a Light when clicked.
          val light = new Light(angle.toInt, (mouseX, mouseY))
          lightsMap += ((mouseX, mouseY) -> light)
        }
        
        case 2 => { //when G is pressed, variable lighting is set to 2, thus proceeding with the simulation
          
          for (i <- lightsMap) {
            i._2.go            
          }
        }
        
        case 0 => { //when placing objects, lighting is set to 0, thus placing an object of chosen type at mouse coordinates, checks for automated, so that clicking for example Load state doesnt also place an object at mouse coordinates.
         if (!automated) placeObj(objIndex, (mouseX, mouseY))
           }
        case 3 => { //when clearing the lights after simulation is finished, lighting is set to 3. 
        lightsMap.clear()
        shines.clear()
        lighting = 0
        updInfo
        }
        
      }   
         for(i <- objectsMap) { //these for loops redraw everything when mouse is clicked so that they do not disappear.
      g.setColor(getColor(i._2))
           g.draw(new Line2D.Double(i._1._1, i._1._2, i._1._3, i._1._4))
      }  
         if (!lightsMap.isEmpty) {
           for (i <- lightsMap) {
             g.setColor(Color.yellow)
             g.fillRect(i._1._1, i._1._2, 3, 3)
           }
         if (lighting == 2) {
           for (i <- shines) g.fillRect(i._1._1, i._1._2, 3, 3)
           lighting = 3
           info.text = "Simulation finished, click anywhere to clear lights"
         }
         }
        automated = false  
      mouseclicked = false
     
    }
    
    
  }
      listenTo(mouse.clicks, keys)
      
      reactions += {
        case MouseClicked(_, p, _, 1, _) => {  
          mouseX = p.x
          mouseY = p.y
          mouseclicked = true 
          
          repaint
        }
        case KeyPressed(_,Key.Z,_,_) => {
          rolls += 1
          updInfo
          info.repaint
        }
        case KeyPressed(_,Key.X,_,_) => {
          angle += 1
          if (angle == 360) angle = 0
          updInfo 
          info.repaint
        }
        case KeyPressed(_,Key.Key0,_,_) => {
          angle = 0
          updInfo
          info.repaint
        }
        case KeyPressed(_,Key.Key1,_,_) => {
          angle = 90
          updInfo
          info.repaint
        }
        case KeyPressed(_,Key.Key2,_,_) => {
          angle = 180
          updInfo
          info.repaint
        }
        case KeyPressed(_,Key.Key3,_,_) => {
          angle = 270
          updInfo
          info.repaint
        }
        case KeyPressed(_,Key.L,_,_) => {
         if (lighting == 1) lighting = 0 else lighting = 1
          updInfo
          info.repaint
        }
        case  KeyPressed(_,Key.G,_,_)  => {
                lighting = 2
               updInfo
               info.repaint
               
        }
        case KeyPressed(_,Key.R,_,_) => {
          randomize
        }
      }
    
      focusable = true
      requestFocus
    
    }
    val bp = new BoxPanel(Orientation.Vertical)

    bp.contents += canvas
    bp.contents += info
    contents = bp
  def clear = { //clears everything and redraws
    objectsMap.clear()
    lightsMap.clear()
    shines.clear()
    lighting = 0
    updInfo
    repaint
  }  
  def clearLights = { //sets the lighting variable to value 3 so that the lights will be cleared
    lighting = 3
    mouseclicked = true
    repaint
  }
  
  def randomize = { //adds 12 random objects on the screen
    val angleWas = angle
    val rand = scala.util.Random
    for(i <- 1 to 12) {
      angle = rand.nextInt(360)
      var x = rand.nextInt(899)
      var y = rand.nextInt(799)
      placeObj(rand.nextInt(4), (x, y))
    }
    angle = angleWas
    automated = true
    mouseclicked = true
    repaint
  }
  
  def preset = { //creates the preset scenario: box
    val angleWas = angle
    angle = 0
    val start = (10, 10)
    placeObj(0, start)
    var end = getEnd(10, 10, 0)
  for(i <- 1 to 8) {
    placeObj(0, end)
    end = getEnd(end._1, end._2, 0)
  }
    angle = 90
    for(i <- 1 to 7) {
      placeObj(0, end)
      end = getEnd(end._1, end._2, 90)
    }
    angle = 180
    for(i <- 1 to 8) {
      placeObj(0, end)
      end = getEnd(end._1, end._2, 180)
    }
    angle = 270
    for(i <- 1 to 6) {
      placeObj(0, end)
      end = getEnd(end._1, end._2, 270)
    }
    angle = 0
    for(i <- 1 to 7) {
      placeObj(0, end)
      end = getEnd(end._1, end._2, 0)
    }
    angle = 90
    for(i <- 1 to 5) {
      placeObj(0, end)
      end = getEnd(end._1, end._2, 90)
    }
    angle = angleWas
    automated = true    
    mouseclicked = true
    repaint
  }
    
  def load = { //simple load file method to load objects from "sav.sv", if such file exists in the directory
    if (new java.io.File("sav.sv").exists()) {
    val file = Source.fromFile("sav.sv")
    val lines = file.getLines.toList
    file.close
    val objs = lines.takeWhile(_ != "#")
    val lights = lines.drop(objs.size + 1)
    for (i <- objs) {
      val split = i.split(":")
      val objType = split(0) .toInt
      val loc = split(1) .drop(1) .dropRight(1) .split(",")
      angle = split(2) .toInt
      placeObj(objType, (loc(0) .toInt, loc(1) .toInt))
    }
    for (i <- lights) {
      val split = i.split(":")
      val loc = split(0) .drop(1) .dropRight(1) .split(",")
      val a = split(1) .toInt
      val light = new Light(a, (loc(0) .toInt, loc(1) .toInt))
      lightsMap += light.location -> light
    }
    automated = true
    mouseclicked = true
    repaint
    }
  }
  
  
  }
  
  def help = { //pops up the help dialog
    Dialog.showMessage(new BoxPanel(Orientation.Vertical), "Welcome to reflections!\n\nPlace reflective objects and atleast one lightsource and start simulation!\nAngle is considered here as a direction from the location of your mouse.\n0: right, 90: down, 180: left, 270: up\n\nKeys:\nZ: cycle through object types\nX: cycle through angles\n0, 1, 2, 3: shortcut to angles 0, 90, 180, 270, respectively\nL: lightsource placement mode\nG: start simulation", title = "Help")
  }
  
  
  
  def mouseSin(num: Int, a: Int) = num + (80 * sin(a.toRadians)) //given x coordinate num and angle a, counts the end x-coordinate.
  def mouseCos(num: Int, a: Int) = num + (80 * cos(a.toRadians)) //given y coordinate num and angle a, counts the end y-coordinate.
  
  
  
  def placeObj(objType: Int, coords: (Int, Int)) = { //method that places objects in the objects map, which are then drawn by the canvas
    val ends = getEnd(coords._1, coords._2, angle.toInt)
    val obj = objType match {
      case 0 => new StraightMirror(this.angle.toInt, coords)
      case 1 => new ConcaveMirror(this.angle.toInt, coords)
      case 2 => new ConcaveLens(this.angle.toInt, coords)
      case 3 => new ConvexLens(this.angle.toInt, coords)
      }

    objectsMap += ((coords._1, coords._2, ends._1, ends._2) -> obj)
  }
  
  def save = { //a very simple save file method that saves and always overwrites "sav.sv" with chosen formatting
    val file = new PrintWriter(new File("sav.sv"))
    for (i <- objectsMap) {
    file.write(i._2.getType + ":" + i._2.getInitial + ":" + i._2.getAngle + "\n")
    }
    file.write("#\n")
    for (i <- lightsMap) {
      file.write(i._1 + ":" + i._2.angle + "\n")
    }
    file.close
  }

  
  def getEnd(x: Int, y: Int, a: Int): (Int, Int) = { //similar to getEndLong in Light. Counts the end coordinates with mouseCos/mouseSin (short versions)
  
  var endX = mouseCos(x, a.toInt)
  var endY = mouseSin(y, a.toInt)
  
  if (endX >= 1000) endX = 999
  if (endX < 0) endX = 0
  if (endY >= 800) endY = 799
  if (endY < 0) endY = 0
  
  (endX.toInt, endY.toInt)
  }
  
  
}