

/**
 * @author owenunderwood_2016
 */
class NumValue(num: Int) extends Value{
  def asStmt = {
    println("Attempt to use a number as a statement")
  }
  
  def asDouble: Int = {
    num
  }
}