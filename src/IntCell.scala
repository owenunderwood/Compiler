

/**
 * @author owenunderwood_2016
 */
class IntCell(int: Int) extends Value {
  var intValue = 0
  
  def set(int: Int) = {
    intValue = int
  }
  
  def get:Int = {
    intValue
  }
}