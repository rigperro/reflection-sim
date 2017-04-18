import scala.math._

class Light(val angle: Int, var location: (Int, Int)) extends Reflective(angle) {
  def newAngle(x: Int) = this.angle 
  lazy val obstacles = Simulator.objectsMap
  private var bounces = 0
  
  def advance: Unit = {
   val target = getEndLong(location._1, location._2, angle)
   var reflected = false
   var ended = false
   var prevLocation = (0, 0)
   if (target == location) {} else {
   val path1 = bresenham(location._1, location._2, target._1, target._2) .toIterator
   
   val path2 = scala.collection.mutable.Set[(Int, Int)]()
  
   while (!reflected && !ended) {
     if(!path1.hasNext) ended = true else {
     val currentLocation = path1.next
     val test = testReflection(currentLocation)
     if (test.isDefined) {
       val obj = test.get
       reflected = true
       
       val newLight = new Light(obj.newAngle(angle), prevLocation)
           newLight.advance
        //   println(obj.angle)
       
      
      // println(obj.getAngle)
     }
     prevLocation = currentLocation
     path2 += currentLocation
      
   }
   }
   path2.foreach(Simulator.addShine(_))
   }
  }
   
  
 
   
 def getEndLong(x: Int, y: Int, a: Int): (Int, Int) = {
  
  var endX = x + ((999) * cos(a.toRadians))
  var endY = y + ((799) * sin(a.toRadians)) 
  
  (endX.toInt, endY.toInt)
  }
  
  def reflects(light: (Int, Int), ref: (Int, Int, Int, Int)): Boolean = {
    val b = bresenham(ref._1, ref._2, ref._3, ref._4) .toSet
    b.contains(light)
   // Simulator.sim(ref._1)(ref._2)
  }
  
  def testReflection(location: (Int, Int)): Option[Reflective] = {
    
    for (i <- obstacles) {
      if ( reflects((location._1, location._2), (i._1._1, i._1._2, i._1._3, i._1._4)) ) return Option(i._2)
    }
    None
  }
  
def element(light: (Int, Int), ref: (Int, Int, Int, Int)): Reflective = {
    val b = bresenham(ref._1, ref._2, ref._3, ref._4) .toSet
    b.contains(light)
   Simulator.sim(ref._1)(ref._2)
  }
    
  
  def bresenham(x0: Int, y0: Int, x1: Int, y1: Int) = {

    val dx = abs(x1 - x0)
    val dy = abs(y1 - y0)

    val sx = if (x0 < x1) 1 else -1
    val sy = if (y0 < y1) 1 else -1

    new Iterator[(Int, Int)] {
      var (x, y) = (x0, y0)
      var err = dx - dy

      def next = {
        val omitted = (x, y)
        val e2 = 2 * err
        if (e2 > -dy) {
          err -= dy
          x += sx
        }
        if (e2 < dx) {
          err += dx
          y += sy
        }
        omitted
      }

      def hasNext = (sx*x <= sx*x1 && sy*y <= sy*y1)
    }
  }
  
}