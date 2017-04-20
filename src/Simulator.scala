
import scala.swing._
import scala.swing.event._
import javax.swing.UIManager
import java.awt.Color
import javax.imageio.ImageIO
import java.io.File
import java.awt.{Graphics2D,Color,Font,BasicStroke}
import java.awt.image.BufferedImage
import java.awt.geom._
import java.awt.BasicStroke._
import scala.math._

object Simulator extends SimpleSwingApplication {
 
  
  val objectsText = Seq[String]("Mirror, straight (white)", "Mirror, concave (red)",  "Lens, concave (green)", "Lens, convex (blue)")
  val objectsMap = scala.collection.mutable.Map[(Int, Int, Int, Int), Reflective]()
  val lightsMap = scala.collection.mutable.Map[(Int, Int), Light]()
  val shines = scala.collection.mutable.Map[(Int, Int), Reflective]()
  def addShine(x: (Int,Int)) = shines += x -> Shine 
  var rolls = 0
  def objIndex = rolls % 4
  
  var angle = 0.0
  
  def updInfo = {
    if (lighting == 1) info.text = "Placing lightsource at angle " + angle.toInt + "°." else if (lighting == 2) info.text = "Click anywhere to start simulation." else
    info.text = "Placing object of type: " + objectsText(objIndex) + " at angle " + angle.toInt + "°."
  }
  
  var lighting = 0
  var lights = 0

  
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
    
 // val sim = Array.ofDim[Reflective](1000,800)
  
  def top = new MainFrame {
    this.menuBar = new MenuBar {
      contents += new Menu("Start") {
        contents += new MenuItem(Action("Help"){help})
        contents += new MenuItem(Action("Add random objects"){randomize})
        contents += new MenuItem(Action("Clear everything"){clear})
        contents += new MenuItem(Action("Clear lights"){clearLights})
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
       
        case 1 => {
          val light = new Light(angle.toInt, (mouseX, mouseY))
         // sim(mouseX)(mouseY) = light
          lightsMap += ((mouseX, mouseY) -> light)
        }
        
        case 2 => {
          for (i <- lightsMap) {
            i._2.go
          }
        }
        
        case 0 => {
          placeObj(objIndex, (mouseX, mouseY))
           }
        case 3 => {
          lighting = 0 
        }
      }   
         for(i <- objectsMap) {
      g.setColor(getColor(i._2))
           g.draw(new Line2D.Double(i._1._1, i._1._2, i._1._3, i._1._4))
      }  
         if (!lightsMap.isEmpty) {
           for (i <- lightsMap) {
             g.setColor(Color.yellow)
             g.fillRect(i._1._1, i._1._2, 3, 3)
           }
           for (i <- shines) g.fillRect(i._1._1, i._1._2, 3, 3)
         }
          
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
          angle = 200
          updInfo
          info.repaint
        }
        case KeyPressed(_,Key.Key3,_,_) => {
          angle = 300
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
      }
    
      focusable = true
      requestFocus
    
    }
    val bp = new BoxPanel(Orientation.Vertical)

    bp.contents += canvas
    bp.contents += info
    contents = bp
  def clear = {
    objectsMap.clear()
    lightsMap.clear()
    shines.clear()
    lighting = 0
    updInfo
    repaint
  }  
  def clearLights = {
    lightsMap.clear()
    shines.clear()
    lighting = 0
    updInfo
    lighting = 3
    mouseclicked = true
    repaint
  }
  
  def randomize = {
    val rand = scala.util.Random
    for(i <- 1 to 12) {
      angle = rand.nextInt(360)
      placeObj(rand.nextInt(4), (rand.nextInt(999), rand.nextInt(799)))
      mouseclicked = true
    }
    repaint
  
  }
  }
  
  def help = {
    Dialog.showMessage(new BoxPanel(Orientation.Vertical), "Help text", title = "Help")
  }
  
  
  
  def mouseSin(num: Int, a: Int) = num + (80 * sin(a.toRadians))
  def mouseCos(num: Int, a: Int) = num + (80 * cos(a.toRadians))
  
  
  
  def placeObj(objType: Int, coords: (Int, Int)) = {
    val ends = getEnd(coords._1, coords._2, angle.toInt)
    val obj = objType match {
      case 0 => new StraightMirror(this.angle.toInt, coords)
      case 1 => new ConcaveMirror(this.angle.toInt, coords)
      case 2 => new ConcaveLens(this.angle.toInt, coords)
      case 3 => new ConvexLens(this.angle.toInt, coords)
      }
   
 //   sim(coords._1)(coords._2) = obj
  //  sim(ends._1)(ends._2) = obj
    objectsMap += ((coords._1, coords._2, ends._1, ends._2) -> obj)
  }
  
  def getEnd(x: Int, y: Int, a: Int): (Int, Int) = {
  
  var endX = mouseCos(x, a.toInt)
  var endY = mouseSin(y, a.toInt)
  
  if (endX >= 1000) endX = 999
  if (endX < 0) endX = 0
  if (endY >= 800) endY = 799
  if (endY < 0) endY = 0
  
  (endX.toInt, endY.toInt)
  }
  
  
}