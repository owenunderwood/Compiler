

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
    println("Attempt to use a procedure as a number")
    sys.exit
  }
  
  def boolValue = {
    println("Attempt to use a procedure as a boolean")
    sys.exit
  }
  
  def getParams:List[Param] = {
    params
  }
  
  def getBlock:Block = {
    block
  }
}