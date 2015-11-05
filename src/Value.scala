

/**
 * @author owenunderwood_2016
 */
trait Value {
  def set(value: Value)
  def boolValue:Boolean
  def intValue:Int
  def procValue:ProcValue
}