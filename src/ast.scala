
case class Program(name: String, block: Block) {
  def render(indent: String): String = {
    var result = indent + "Program " + name + "\n";
    result = result + block.render(indent + "  ")
    result
  }
}

case class Block(consts: List[ConstDecl], vars: List[VarDecl], procs: List[ProcDecl], body: List[Stmt]) {
  def render(indent: String): String = {
    var result = indent + "Block\n"
    if (!consts.isEmpty) {
      for (decl <- consts) {
        result = result + decl.render(indent + "  ")
      }
    }
    if (!vars.isEmpty) {
      for (decl <- vars) {
        result = result + decl.render(indent + "  ")
      }
    }
    if (!procs.isEmpty) {
      for (decl <- procs) {
        result = result + decl.render(indent + "  ")
      }
    }
    if (!body.isEmpty) {
      for (decl <- body) {
        result = result + decl.render(indent + "  ")
      }
    }
    return result;
  }
}

case class ConstDecl(id: String, value: Int) {
  def render(indent: String): String = {
    indent + "Const " + id + " = " + value + "\n"
  }
}

case class VarDecl(id: String, typ: Type) {
  def render(indent: String): String = {
    var result = indent + "Var " + id + " : " + typ + "\n"
    result
  }
}

trait Type {
  def render(indent: String): String
}
case object IntType extends Type {
  def render(indent: String): String = {
    var result = "Int"
    result
  }
}
case object BoolType extends Type {
  def render(indent: String): String = {
    var result = "Bool"
    result
  }
}

case class ProcDecl(id: String, params: List[Param], block: Block) {
  def render(indent: String): String = {
    var result = indent + "Proc " + id + "\n"
    if (!params.isEmpty) {
      for (param <- params) {
        if (param.getClass == ValParam) {
          var result = indent + "Val"
          result = result + id + " : " + param.getType + "\n"
          result
        } else {
          var result = indent + "Var"
          result = result + id + " : " + param.getType + "\n"
          result
        }
      }
    }
    result = result + block.render(indent + "   ")
    result
  }
}

trait Param {
  def getType: Type
}
case class ValParam(id: String, typ: Type) extends Param {
  override def getType: Type = {
    typ
  }
}
case class VarParam(id: String, typ: Type) extends Param {
  override def getType: Type = {
    typ
  }
}

trait Stmt {
  def render(indent: String): String
}
case class Assign(id: String, expr: Expr) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "Assign" + id + "\n"
    result = result + expr.render(indent + "  ")
    result
  }
}
case class Call(id: String, args: List[Expr]) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "Call " + id + "\n"
    if (!args.isEmpty) {
      for (arg <- args) {
        result = result + arg.render(indent + "  ")
      }
    }
    result
  }
}
case class Sequence(body: List[Stmt]) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "Sequence\n"
    if (!body.isEmpty) {
      for (stmt <- body) {
        result = result + stmt.render(indent + "  ")
      }
    }
    result
  }
}
case class IfThen(test: Expr, trueClause: Stmt) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "IfThen\n"
    result = result + test.render(indent + "  ")
    result = result + trueClause.render(indent + "  ")
    result
  }
}
case class IfThenElse(test: Expr, trueClause: Stmt, falseClause: Stmt) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "IfThenElse\n"
    result = result + test.render(indent + "  ")
    result = result + trueClause.render(indent + "  ")
    result = result + falseClause.render(indent + "  ")
    result
  }
}
case class While(test: Expr, body: Stmt) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "While\n"
    result = result + test.render(indent + "  ")
    result = result + body.render(indent + "  ")
    result
  }
}
case class Prompt(message: String) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "Prompt \"" + message + "\"\n"
    result
  }
}
case class Prompt2(message: String, id: String) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "Prompt2 \"" + message + "\", " + id + "\n"
    result
  }
}
case class Print(items: List[Item]) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "Print\n"
    if (!items.isEmpty) {
      for (item <- items) {
        result = result + item.render(indent + "  ")
      }
    }
    result
  }
}

trait Item {
  def render(indent: String): String
}
case class ExprItem(expr: Expr) extends Item {
  def render(indent: String): String = {
    var result = indent + "ExprItem\n"
    result = result + expr.render(indent + "  ")
    result
  }
}
case class StringItem(message: String) extends Item {
  def render(indent: String): String = {
    var result = indent + "StringItem \"" + message + "\"\n"
    result
  }
}

trait Expr {
  def render(indent: String): String
}
case class BinOp(left: Expr, op: Op2, right: Expr) extends Expr {
  def render(indent: String): String = {
    var result = indent + "BinOp " + op + "\n"
    result = result + left.render(indent + "  ")
    result = result + right.render(indent + "  ")
    result
  }
}
case class UnOp(op: Op1, expr: Expr) extends Expr {
  def render(indent: String): String = {
    var result = indent + "UnOp " + op + "\n"
    result = result + expr.render(indent + "  ")
    result
  }
}
case class Num(value: Int) extends Expr {
  def render(indent: String): String = {
    indent + "Num " + value + "\n"
  }
}
case class Id(id: String) extends Expr {
  def render(indent: String): String = {
    indent + "Id " + id + "\n"
  }
}
case object True extends Expr {
  def render(indent: String): String = {
    "True"
  }
}
case object False extends Expr {
  def render(indent: String): String = {
    "False"
  }
}

trait Op2
case object EQ extends Op2 {
  def render(indent: String): String = {
    "EQ"
  }
}
case object NE extends Op2 {
  def render(indent: String): String = {
    "NE"
  }
}
case object LE extends Op2 {
  def render(indent: String): String = {
    "LE"
  }
}
case object GE extends Op2 {
  def render(indent: String): String = {
    "GE"
  }
}
case object LT extends Op2 {
  def render(indent: String): String = {
    "LT"
  }
}
case object GT extends Op2 {
  def render(indent: String): String = {
    "GT"
  }
}
case object Plus extends Op2 {
  def render(indent: String): String = {
    "Plus"
  }
}
case object Minus extends Op2 {
  def render(indent: String): String = {
    "Minus"
  }
}
case object Times extends Op2 {
  def render(indent: String): String = {
    "Times"
  }
}
case object Div extends Op2 {
  def render(indent: String): String = {
    "Div"
  }
}
case object Mod extends Op2 {
  def render(indent: String): String = {
    "Mod"
  }
}
case object And extends Op2 {
  def render(indent: String): String = {
    "And"
  }
}
case object Or extends Op2 {
  def render(indent: String): String = {
    "Or"
  }
}

trait Op1
case object Neg extends Op1 {
  def render(indent: String): String = {
    "Neg"
  }
}
case object Not extends Op1 {
  def render(indent: String): String = {
    "Not"
  }
}