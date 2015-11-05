

/**
 * @author owenunderwood_2016
 */
class IntValue(int: Int) extends Value{
  
  def intValue:Int = {
    int
  }
  
  def set(v: Value) = {
    println("Attempt to set a const")
  }
  
  def boolValue = {
    println("Attempt to use an int as a boolean")
    sys.exit
  }
  
  def procValue = {
    println("Attempt to use an int as a procedure")
    sys.exit
  }
}