

/**
 * @author owenunderwood_2016
 */
class IntCell(int: Int) extends Value {
  var intValue = int
  
  def set(v: Value) = {
    intValue = v.intValue
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