import scala.collection.mutable.Stack
import scala.collection.mutable.Map
/**
 * @author owenunderwood_2016
 */
class SymbolTable {
  val scopes = Stack[collection.mutable.Map[String, Value]]()
  
  def enter(id: String) = {
    scopes.push(collection.mutable.Map[String, Value]())
  }

  def exit = {
    scopes.pop
  }

  def contains(id: String): Boolean = {
    if (scopes.top.contains(id)) {
      true
    }
    else {
      false
    }
  }
  
  def bind(id: String, typ: Value) = {
    scopes.top += (id -> typ)
  }

  def lookup(id: String) = {
    for (map <- scopes) {
      if (map.contains(id)) {
        map(id)
      } else {
        println("Unkown variable " + id)
        null
      }
    }
  }
}