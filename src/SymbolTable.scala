import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import scala.util.control.Breaks._

/**
 * @author owenunderwood_2016
 */
class SymbolTable {
  var scopes = Stack[collection.mutable.Map[String, Value]]()

  def enter(id: String) = {
    scopes.push(collection.mutable.Map[String, Value]())
  }

  def exit = {
    scopes.pop
  }

  def contains(id: String): Boolean = {
    if (scopes.top.contains(id)) {
      true
    } else {
      false
    }
  }

  def bind(id: String, typ: Value) = {
    scopes.top += (id -> typ)
  }
  
  def lookup(id: String): Value = {
    var res: Value = null
    breakable {
      for (scope <- scopes) {
        if (scope.contains(id)) {
          res = scope(id)
          break
        }
      }
    }
    res
  }
  
  //def lookup(id: String): Value = {
  //  var res: Value = null
  //  breakable {
  //    for (i <- scopes.size-1 to 0) {
  //      if (scopes(i).contains(id)) {
  //        res = scopes(i).getOrElse(id, null)
  //        break
  //     } else {
  //       println("Unkown variable " + id)
  //     }
  //    }
  //  }
  //  res
  //}
}