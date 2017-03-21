
import scala.swing._
import scala.swing.event._
import java.awt.Color
import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage

object Simulator extends SimpleSwingApplication {
 
    
  val objects = Seq[String]("Mirror, concave", "Mirror, straight", "Lens, concave", "Lens, convex")
  var rolls = 0
  def updInfo = info.text = "Placing object of type " + objects(rolls % 4)
    
  
  val info = new Label
  updInfo
    
  
  
  def top = new MainFrame {
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
      g.drawImage(m1, mouseX - 10, mouseY - 10, null)
      mouseclicked = false
    }
  }
      listenTo(mouse.clicks)
      listenTo(mouse.wheel)
      reactions += {
        case MouseClicked(_, p, _, 1, _) => {  
          mouseX = p.x
          mouseY = p.y
          mouseclicked = true 
          repaint
        }
        case MouseWheelMoved(_,_,_,_) => {
          rolls += 1
          updInfo
          repaint
        }
      }
     
    }
    val bp = new BoxPanel(Orientation.Vertical)

    bp.contents += canvas
    bp.contents += info
    contents = bp
  }
  
  // Images
  
  def makeImg(fileName: String): BufferedImage = ImageIO.read(new File(fileName))
		
  
  val m1 = makeImg("img/mconcave.png")
  val m2 = makeImg("img/mstraight.png")
  val l1 = makeImg("img/lconcave.png")
  val l2 = makeImg("img/lconvex.png")
  
  
}