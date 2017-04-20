

abstract class Reflective(angle: Int, initial: (Int, Int)) {
   def getAngle = this.angle
   def getInitial = this.initial
   def newAngle(lightAngle: Int) :Int
   var proximity: Double
    def concave = {
    val a = if(proximity < 40) 41 - proximity else proximity - 39
     val b = a / 2
    if (proximity > 40) b else -b 
   }
}

class ConcaveMirror(angle: Int, initial: (Int, Int)) extends Reflective(angle, initial) {
    def newAngle(lightAngle: Int) = {
      val conc = if (lightAngle < 270 && lightAngle > 90) - concave else concave
      (360 - lightAngle + 2 * (this.angle % 180)) + conc .toInt % 360
    }
    var proximity = 0.0

    
}

class StraightMirror(angle: Int, initial: (Int, Int)) extends Reflective(angle, initial) {
   def newAngle(lightAngle: Int) = (360 - lightAngle + 2 * (this.angle % 180)) % 360
  var proximity = 0.0
}

class ConvexLens(angle: Int, initial: (Int, Int)) extends Reflective(angle, initial) {
  def newAngle(lightAngle: Int) = {
    val conc = if (lightAngle < 270 && lightAngle > 90) - concave else concave
      (lightAngle - conc.toInt) % 360
    }
  var proximity = 0.0
}

class ConcaveLens(angle: Int, initial: (Int, Int)) extends Reflective(angle, initial) {
    
    def newAngle(lightAngle: Int) = {
      val conc = if (lightAngle < 270 && lightAngle > 90) - concave else concave
      (lightAngle + conc.toInt) % 360
    }
    var proximity = 0.0
}

object Shine extends Reflective(0, (0,0)) {
   def newAngle(lightAngle: Int) = 0
   var proximity = 0.0
}
