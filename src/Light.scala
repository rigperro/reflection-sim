import scala.math._

class Light(var angle: Int, var location: (Int, Int)) {
   
  def edge = this.location._1 < 1 || this.location._1 > 999 || this.location._2 < 1 || this.location._2 > 799
  
  
   lazy val objects = {
    val a = scala.collection.mutable.HashMap[(Int, Int), Reflective]()
    for (i <- Simulator.objectsMap) {
      val bres = bresenham(i._1._1, i._1._2, i._1._3, i._1._4)
      for(j <- bres) {
        a += j -> i._2
        a += (min(999, j._1 + 1), j._2) -> i._2
      }
    }
    a
  }
  
  def go = this.advance(scala.collection.mutable.Set[(Int, Int)](), angle, 0)
  
  
  def advance(path: scala.collection.mutable.Set[(Int, Int)], pathAngle: Int, bounces: Int): Unit = {
    val target = getEndLong(location._1, location._2, pathAngle)
  val p = scala.collection.mutable.Set[(Int, Int)]()
  if (edge || bounces > 100) path.foreach(Simulator.addShine(_)) else {
    val currentPath = bresenham(location._1, location._2, target._1, target._2) 
    var stop = false
    
    while (!stop && currentPath.hasNext) {
     val currentLocation = currentPath.next
     val test = objects.get(currentLocation)
     if (test.isDefined && p.size > 3) {
       val obj = test.get
       stop = true 
       obj.proximity = distance(obj.getInitial)
      if(obj.getAngle != pathAngle) advance(p, obj.newAngle(pathAngle), bounces + 1)
     }
     this.location = currentLocation
     p += currentLocation
    }
    p.foreach(Simulator.addShine(_))
  }
  }
   
  def distance(to: (Int, Int)):  Int = {
   abs( sqrt(pow(location._1 - to._1, 2) + pow(location._2 - to._2, 2))) .toInt
  }
 
   
 def getEndLong(x: Int, y: Int, a: Int): (Int, Int) = {
  
  val endX = x + ((999) * cos(a.toRadians))
  val endY = y + ((799) * sin(a.toRadians)) 
  
  (endX.toInt, endY.toInt)
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