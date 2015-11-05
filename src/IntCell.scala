

/**
 * @author owenunderwood_2016
 */
class IntCell(int: Int) extends Value {
  var intValue = 0
  
  def set(v: Value) = {
    intValue = v.intValue
  }
  
  def get:Int = {
    intValue
  }

   def boolValue = {
    println("Attempt to use an int as a bool")
    sys.exit()
  }

  def procValue = {
    println("Attempt to use an int as a proc")
    sys.exit()
  }

}