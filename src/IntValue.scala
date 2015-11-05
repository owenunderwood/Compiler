

/**
 * @author owenunderwood_2016
 */
class IntValue(int: Int) extends Value{
  
  def intValue:Int = {
    int
  }
  
  def set(v: Value) = {
    
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