case class Program(name: String, block: Block)

case class Block(consts: List[ConstDecl], vars: List[VarDecl], procs: List[ProcDecl], body: List[Stmt])

case class ConstDecl(id: String, value: Int)

case class VarDecl(id: String, typ: Type)

trait Type
case object IntType extends Type
case object BoolType extends Type

case class ProcDecl(id: String, params: List[Param], block: Block)

trait Param
case class ValParam(id: String, typ: Type) extends Param
case class VarParam(id: String, typ: Type) extends Param

trait Stmt
case class Assign(id: String, expr: Expr) extends Stmt
case class Call(id: String, args: List[Expr]) extends Stmt
case class Sequence(body: List[Stmt]) extends Stmt
case class IfThen(test: Expr, trueClause: Stmt) extends Stmt
case class IfThenElse(test: Expr, trueClause: Stmt, falseClause: Stmt) extends Stmt
case class While(test: Expr, body: Stmt) extends Stmt
case class Prompt(message: String) extends Stmt
case class Prompt2(message: String, id: String) extends Stmt
case class Print(items: List[Item]) extends Stmt

trait Item
case class ExprItem(expr: Expr) extends Item
case class StringItem(message: String) extends Item

trait Expr
case class BinOp(left: Expr, op: Op2, right: Expr) extends Expr
case class UnOp(op: Op1, expr: Expr) extends Expr
case class Num(value: Int) extends Expr
case class Id(id: String) extends Expr
case object True extends Expr
case object False extends Expr

trait Op2
case object EQ extends Op2
case object NE extends Op2
case object LE extends Op2
case object GE extends Op2
case object LT extends Op2
case object GT extends Op2
case object Plus extends Op2
case object Minus extends Op2
case object Times extends Op2
case object Div extends Op2
case object Mod extends Op2
case object And extends Op2
case object Or extends Op2

trait Op1
case object Neg extends Op1
case object Not extends Op1