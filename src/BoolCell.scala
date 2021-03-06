

/**
 * @author owenunderwood_2016
 */
class BoolCell(bool: Boolean) extends Value {
  var boolValue = bool
  
  def set(v: Value) = {
    if (v.boolValue == true) {
      boolValue = true
    }
    else {
      boolValue = false
    }
  }
  
  def intValue = {
    println("int /= BoolCell")
    sys.exit()
  }

  def procValue = {
    println("proc /= BoolCell")
    sys.exit()
  }
}