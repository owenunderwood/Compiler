

/**
 * @author owenunderwood_2016
 */
class BoolCell(bool: Boolean) extends Value {
  var boolValue = false
  
  def set(v: Value) = {
    if (v.boolValue == true) {
      boolValue = true
    }
    else {
      boolValue = false
    }
  }
  
  def get:Boolean = {
    boolValue
  }
  
  def intValue:Int = {
    println("int /= BoolCell")
    sys.exit()
  }

  def procValue = {
    println("proc /= BoolCell")
    sys.exit()
  }
}