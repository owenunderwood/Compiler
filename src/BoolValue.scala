
class BoolValue(bool: Boolean) extends Value{

  def set(v: Value) {
    
  }
  
  def boolValue = {
    bool
  }
  
  def intValue = {
    println("int /= BoolValue")
    sys.exit()
  }

  def procValue = {
    println("proc /= BoolValue")
    sys.exit()
  }
}
