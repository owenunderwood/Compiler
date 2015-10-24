
case class Program(name: String, block: Block) {
  override def toString = "Program " + name + '\n' + block
}

case class Block(consts: List[ConstDecl], vars: List[VarDecl], procs: List[ProcDecl], body: List[Stmt]){
  override def toString = "Block" + '\n' + consts + vars + procs + body
}

case class ConstDecl(id: String, value: Int) {
  override def toString = "Const " + id + " = " + value + '\n'
}

case class VarDecl(id: String, typ: Type) {
  override def toString = "Var " + id + " : " + typ + '\n'
}

trait Type
case object IntType extends Type {
  override def toString = "Int"
}
case object BoolType extends Type {
  override def toString = "Bool"
}

case class ProcDecl(id: String, params: List[Param], block: Block) {
  override def toString = "Proc " + id + '\n' + params + '\n' + block
}
  
trait Param
case class ValParam(id: String, typ: Type) extends Param {
  override def toString = "Val " + id + " : " + typ
}
case class VarParam(id: String, typ: Type) extends Param {
  override def toString = "Var " + id + " : " + typ
}

trait Stmt
case class Assign(id: String, expr: Expr) extends Stmt {
  override def toString = "Assign " + id + '\n' + expr
}
case class Call(id: String, args: List[Expr]) extends Stmt {
  override def toString = "Call " + id + '\n' + args
}
case class Sequence(body: List[Stmt]) extends Stmt {
  override def toString = "Sequence" + '\n' + body
}
case class IfThen(test: Expr, trueClause: Stmt) extends Stmt {
  override def toString = "IfThen " + test + '\n' + trueClause
}
case class IfThenElse(test: Expr, trueClause: Stmt, falseClause: Stmt) extends Stmt {
  override def toString = "IfThenElse " + test + '\n' + trueClause + '\n' + falseClause
}
case class While(test: Expr, body: Stmt) extends Stmt {
  override def toString = "While " + '\n' + body
}
case class Prompt(message: String) extends Stmt {
  override def toString = "Prompt " + message + '\n'
}
case class Prompt2(message: String, id: String) extends Stmt {
  override def toString = "Prompt " + message + ", " + id + '\n'
}
case class Print(items: List[Item]) extends Stmt {
  override def toString = "Print " + items + '\n'
}

trait Item 
case class ExprItem(expr: Expr) extends Item {
    override def toString = "ExprItem" + '\n' + expr
}
case class StringItem(message: String) extends Item {
    override def toString = "StringItem" + '\n' + message
}

trait Expr
case class BinOp(left: Expr, op: Op2, right: Expr) extends Expr {
    override def toString = "BinOp" + op + '\n' + left + right
}
case class UnOp(op: Op1, expr: Expr) extends Expr {
    override def toString = "UnOp" + op + '\n' + expr
}
case class Num(value: Int) extends Expr {
    override def toString = "Num " + value + '\n'
}
case class Id(id: String) extends Expr {
    override def toString = "Id " + id + '\n'
}
case object True extends Expr {
    override def toString = "True"
}
case object False extends Expr {
    override def toString = "False"
}

trait Op2
case object EQ extends Op2 {
    override def toString = "EQ"
}
case object NE extends Op2 {
    override def toString = "NE"
}
case object LE extends Op2 {
    override def toString = "LE"
}
case object GE extends Op2 {
    override def toString = "GE"
}
case object LT extends Op2 {
    override def toString = "LT"
}
case object GT extends Op2 {
    override def toString = "GT"
}
case object Plus extends Op2 {
    override def toString = "Plus"
}
case object Minus extends Op2 {
    override def toString = "Minus"
}
case object Times extends Op2 {
    override def toString = "Times"
}
case object Div extends Op2 {
    override def toString = "Div"
}
case object Mod extends Op2 {
    override def toString = "Mod"
}
case object And extends Op2 {
    override def toString = "And"
}
case object Or extends Op2 {
    override def toString = "Or"
}

trait Op1
case object Neg extends Op1 {
    override def toString = "Neg"
}
case object Not extends Op1 {
    override def toString = "Not"
}