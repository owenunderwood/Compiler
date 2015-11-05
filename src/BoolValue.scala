
class BoolValue(bool: Boolean) extends Value{

  def set(v: Value) {
    println("Attempt to set a const")
  }
  
  def boolValue:Boolean = {
    this.bool
  }
  
  def intValue:Int = {
    println("Attempt to use a number as a statement")
    sys.exit()
  }

  def procValue = {
    println("Attempt to use a procedure as a boolean")
    sys.exit()
  }
}
