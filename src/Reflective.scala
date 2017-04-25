

abstract class Reflective(angle: Int, initial: (Int, Int)) {
   def getAngle = this.angle
   def getInitial = this.initial
   def getType: Int //object type in an int 0 to 3, for save/load file purposes
   def newAngle(lightAngle: Int) :Int //called from Light to determine new angle for that Light
   var proximity: Double  //this variable is set from Light
    def concave = { //with proximity counts the amount added or substracted to the new angle for concave/convex
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

    def getType = 1
    
}

class StraightMirror(angle: Int, initial: (Int, Int)) extends Reflective(angle, initial) {
   def newAngle(lightAngle: Int) = (360 - lightAngle + 2 * (this.angle % 180)) % 360
  var proximity = 0.0
  def getType = 0
}

class ConvexLens(angle: Int, initial: (Int, Int)) extends Reflective(angle, initial) {
  def newAngle(lightAngle: Int) = {
    val conc = if (lightAngle < 270 && lightAngle > 90) - concave else concave
      (lightAngle - conc.toInt) % 360
    }
  var proximity = 0.0
  def getType = 3
}

class ConcaveLens(angle: Int, initial: (Int, Int)) extends Reflective(angle, initial) {
    
    def newAngle(lightAngle: Int) = {
      val conc = if (lightAngle < 270 && lightAngle > 90) - concave else concave
      (lightAngle + conc.toInt) % 360
    }
    var proximity = 0.0
    def getType = 2
}

class Shine //initially extended Reflective, thus here 
