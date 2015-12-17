

/**
 * @author owenunderwood_2016
 */
class IntValue(int: Int) extends Value{
  
  def intValue:Int = {
    int
  }
  
  def set(v: Value) = {
    println("Attempt to set a Val")
  }
  
  def boolValue = {
    println("bool /= IntValue")
    sys.exit
  }
  
  def procValue = {
    println("proc /= IntValue")
    sys.exit
  }
}