

abstract class Reflective(angle: Int) {
   def getAngle = this.angle
   def newAngle(lightAngle: Int) :Int
}

class ConcaveMirror(angle: Int) extends Reflective(angle) {
  
  def newAngle(lightAngle: Int) = (360 - lightAngle + 2 * (this.angle % 180)) % 360
}

class StraightMirror(angle: Int) extends Reflective(angle) {
   def newAngle(lightAngle: Int) = (360 - lightAngle + 2 * (this.angle % 180)) % 360
}

class ConvexLens(angle: Int) extends Reflective(angle) {
  def newAngle(lightAngle: Int) = (360 - lightAngle + 2 * (this.angle % 180)) % 360
}

class ConcaveLens(angle: Int) extends Reflective(angle) {
    def newAngle(lightAngle: Int) = (360 - lightAngle + 2 * (this.angle % 180)) % 360
}

object Shine extends Reflective(0) {
   def newAngle(lightAngle: Int) = 0
 }
