

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
    println("bool /= IntCell")
    sys.exit()
  }

  def procValue = {
    println("proc /= IntCell")
    sys.exit()
  }

}