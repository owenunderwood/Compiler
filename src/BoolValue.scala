
class BoolValue(bool: Boolean) extends Value{

  def set(v: Value) {
    println("Attempt to set a const")
  }
  
  def boolValue:Boolean = {
    this.bool
  }
  
  def intValue:Int = {
    println("int /= BoolValue")
    sys.exit()
  }

  def procValue = {
    println("proc /= BoolValue")
    sys.exit()
  }
}
