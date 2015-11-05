

/**
 * @author owenunderwood_2016
 */
class IntValue(intValue: Int) extends Value{
  
  def boolValue = {
    println("Attempt to use an int as a boolean")
    sys.exit
  }
  
  def procValue = {
    println("Attempt to use an int as a procedure")
    sys.exit
  }
}