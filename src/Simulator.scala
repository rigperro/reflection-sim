
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
 
  
  val objectsText = Seq[String]("Mirror, straight (black)", "Mirror, concave (red)",  "Lens, concave (green)", "Lens, convex (blue)")
  val objectsSeq = Seq[Reflective](StraightMirror, ConcaveMirror, ConcaveLens, ConvexLens) 
  val objectsMap = scala.collection.mutable.Map[(Int, Int, Int, Int), Reflective]()
  
  var rolls = 0
  def objIndex = rolls % 4
  
  var angle = 0.0
  def updInfo = {
    if (lighting) info.text = "Placing lightsource at angle " + angle.toInt + "°." else
    info.text = "Placing object of type: " + objectsText(objIndex) + " at angle " + angle.toInt + "°."
  }
  
  var lighting = false
  
  val colors = Seq[Color](Color.black, Color.red, Color.green, Color.blue)
  
  val info = new Label
  updInfo
    
  val sim = Array.ofDim[Reflective](1000,800)
  
  def top = new MainFrame {
    this.menuBar = new MenuBar {
      contents += new Menu("Start") {
        contents += new MenuItem(Action("Help"){help})
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
  background = Color.white
  override def paintComponent(g: java.awt.Graphics2D) {
    super.paintComponent(g);
    if (mouseclicked) {
      
      placeObj(objectsSeq(objIndex), (mouseX, mouseY))
      for(i <- objectsMap) {
      g.setColor(colors(objectsSeq.indexOf(i._2)))
      g.draw(new Line2D.Double(i._1._1, i._1._2, i._1._3, i._1._4))
      }    
      
     println(objectsMap)
      
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
          angle = 100
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
         if (lighting) lighting = false else lighting = true
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
    
    
  }
  
  def help = {
    Dialog.showMessage(new BoxPanel(Orientation.Vertical), "Help text", title = "Help")
  }
  
  def mouseSin(num: Int) = num + (80 * sin(angle.toRadians))
  def mouseCos(num: Int) = num + (80 * cos(angle.toRadians))
  
  def placeObj(objType: Reflective, coords: (Int, Int)) = {
    val ends = getEnd(coords._1, coords._2)
    sim(coords._1)(coords._2) = objType
    sim(ends._1)(ends._2) = objType
    objectsMap += ((coords._1, coords._2, ends._1, ends._2) -> objType)
  }
  
  def getEnd(x: Int, y: Int): (Int, Int) = {
  
  var endX = mouseCos(x)
  var endY = mouseSin(y)
  
  if (endX >= 1000) endX = 999
  if (endX < 0) endX = 0
  if (endY >= 800) endY = 799
  if (endY < 0) endY = 0
  
  (endX.toInt, endY.toInt)
  }
  
  // Images
  
  def makeImg(fileName: String): BufferedImage = ImageIO.read(new File(fileName))
		
  
  val m1 = makeImg("img/mconcave.png")
  val m2 = makeImg("img/mstraight.png")
  val l1 = makeImg("img/lconcave.png")
  val l2 = makeImg("img/lconvex.png")
  
  
  
}