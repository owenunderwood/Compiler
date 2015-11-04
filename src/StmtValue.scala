

/**
 * @author owenunderwood_2016
 */
class StmtValue(stmt: Stmt) extends Value{
  def asDouble = {
    println("Attempt to use statement as a number")
  }
  
  def asStmt: Stmt = {
    stmt
  }
}