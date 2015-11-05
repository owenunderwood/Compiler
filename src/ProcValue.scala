

/**
 * @author owenunderwood_2016
 */
class ProcValue(params: List[Param], block: Block) extends Value{
  
  def set(v: Value) = {
    println("Attempt to set a const")
  }
  
  def procValue:ProcValue = {
    this
  } 
  
  def intValue = {
    println("int /= ProcValue")
    sys.exit
  }
  
  def boolValue = {
    println("bool /= ProcValue")
    sys.exit
  }
  
  def getParams:List[Param] = {
    params
  }
  
  def getBlock:Block = {
    block
  }
}