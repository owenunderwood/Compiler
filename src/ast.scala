

case class Program(name: String, block: Block) {
  def render(indent: String): String = {
    var result = indent + "Program " + name + "\n";
    result = result + block.render(indent + "  ")
    result
  }
  def interpret = {
    var t = new SymbolTable
    t.enter(name)
    block.interpret(t)
    t.exit
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
    result
  }
  
  def interpret(t: SymbolTable) = {
    if (!consts.isEmpty) {
      for (decl <- consts) {
        decl.interpret(t)
      }
    }
    if (!vars.isEmpty) {
      for (decl <- vars) {
        decl.interpret(t)
      }
    }
    if (!procs.isEmpty) {
      for (decl <- procs) {
        decl.interpret(t)
      }
    }
    if (!body.isEmpty) {
      for (decl <- body) {
        decl.interpret(t)
      }
    }

  }
  
}

case class ConstDecl(id: String, value: Int) {
  def render(indent: String): String = {
    indent + "Const " + id + " = " + value + "\n"
  }
  
  def interpret(t: SymbolTable) = {
    if (!t.contains(id)) {
    t.bind(id, new IntValue(value))
  }
    else {
      println("Variable " + id + " is already bound")}
    }
}

case class VarDecl(id: String, typ: Type) {
  def render(indent: String): String = {
    var result = indent + "Var " + id + " : " + typ + "\n"
    result
  }
  
  def interpret(t: SymbolTable) = {
    if (!t.contains(id)) {
      if (typ.equals(IntType)) {
        t.bind(id, new IntCell)
      }
      else {
        t.bind(id, new BoolCell)
      }
  }
    else {
      println("Variable " + id + " is already bound")}
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
  
  def interpret(t: SymbolTable) = {
    if (!t.contains(id)) {
    t.bind(id, new ProcValue(params, block))
  }
    else {
      println("Variable " + id + " is already bound")}
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
  def interpret(t: SymbolTable)
}
case class Assign(id: String, expr: Expr) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "Assign" + id + "\n"
    result = result + expr.render(indent + "  ")
    result
  }
  
  def interpret(t: SymbolTable) = {
    val lhs = t.lookup(id)
    val rhs = expr.interpret(t)
    if (rhs.isInstanceOf[BoolCell]) {
      lhs.set(rhs)
    }
    else {
      lhs.set(rhs)
    }
  }    
}
case class Call(id: String, args: List[Expr]) extends Stmt {
  def call(params: List[Param], block:Block, args: List[Value], t:SymbolTable):Unit = (params, block, args, t)match {    
    case(Nil, block, Nil, t)=> block.interpret(t)
    case(VarParam(id, IntType) :: ps, block, a :: as, t) => 
      t.bind(id, new IntCell)
      call(ps, block, as, t)
    case(VarParam(id, BoolType) :: ps, block, a :: as, t) =>
      t.bind(id, new BoolCell)
      call(ps, block, as, t)
    case(ValParam(id, IntType) :: ps, block, a :: as, t) =>
      t.bind(id, a)
      call(ps, block, as, t)
    case(ValParam(id, BoolType) :: ps, block, a :: as, t) =>
      t.bind(id, a)
      call(ps, block, as, t)
    case _ => 
  }
  
  def render(indent: String): String = {
    var result = indent + "Call " + id + "\n"
    if (!args.isEmpty) {
      for (arg <- args) {
        result = result + arg.render(indent + "  ")
      }
    }
    result
  }
  def interpret(t: SymbolTable) = {
    val P:ProcValue = t.lookup(id).procValue
    val a = for (arg <- args) yield {
      arg.interpret(t)
    } 
    t.enter(id)   
    call(P.getParams, P.getBlock, a, t)
    t.exit
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
  def interpret(t: SymbolTable) = {
    for (s <- body) yield {
      s.interpret(t)
    }
  }
}
case class IfThen(test: Expr, trueClause: Stmt) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "IfThen\n"
    result = result + test.render(indent + "  ")
    result = result + trueClause.render(indent + "  ")
    result
  }
  
  def interpret(t: SymbolTable) = {
    val tst = test.interpret(t)
    if (tst.boolValue) {
      trueClause.interpret(t)
    }
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
  def interpret(t: SymbolTable) = {
    val tst = test.interpret(t)
    if (tst.boolValue) {
      trueClause.interpret(t)
    }
    else {
      falseClause.interpret(t)
    }
  }
}
case class While(test: Expr, body: Stmt) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "While\n"
    result = result + test.render(indent + "  ")
    result = result + body.render(indent + "  ")
    result
  }
  
  def interpret(t: SymbolTable) = {
    var tst = test.interpret(t)
    while (tst.boolValue) {
      body.interpret(t)
      tst = test.interpret(t)
    }
  }
}
case class Prompt(message: String) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "Prompt \"" + message + "\"\n"
    result
  }
  def interpret(t: SymbolTable) = {
    print(message)
    readLine
  }
}
case class Prompt2(message: String, id: String) extends Stmt {
  def render(indent: String): String = {
    var result = indent + "Prompt2 \"" + message + "\", " + id + "\n"
    result
  }
  def interpret(t: SymbolTable) = {
    val lhs = t.lookup(id)
    print(message + " ")
    val input = readLine.toInt
    lhs.set(new IntValue(input))
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
  def interpret(t: SymbolTable) = {
   for(item <- items) {
     item.interpret(t)
   }
   println()
  }
}

trait Item {
  def render(indent: String): String
  def interpret(t: SymbolTable)
}
case class ExprItem(expr: Expr) extends Item {
  def render(indent: String): String = {
    var result = indent + "ExprItem\n"
    result = result + expr.render(indent + "  ")
    result
  }
  def interpret(t: SymbolTable) = {
    val v = expr.interpret(t)
    print(v.intValue)
  }
}
case class StringItem(message: String) extends Item {
  def render(indent: String): String = {
    var result = indent + "StringItem \"" + message + "\"\n"
    result
  }
  def interpret(t: SymbolTable) = {
    print(message)
  }
}

trait Expr {
  def render(indent: String): String
  def interpret(t: SymbolTable): Value
}
case class BinOp(left: Expr, op: Op2, right: Expr) extends Expr {
  def render(indent: String): String = {
    var result = indent + "BinOp " + op + "\n"
    result = result + left.render(indent + "  ")
    result = result + right.render(indent + "  ")
    result
  }
  def interpret(t: SymbolTable):Value = {
    val lhs = left.interpret(t)
    val rhs = right.interpret(t)
    switchOp(lhs, op, rhs)
  }
  def switchOp(lhs: Value, op: Op2, rhs: Value):Value = op match {
      case(And) => new BoolValue(lhs.boolValue && rhs.boolValue)
      case(Or) =>  new BoolValue(lhs.boolValue | rhs.boolValue)
      case(EQ) =>  new BoolValue(lhs.intValue == rhs.intValue)
      case(NE) =>  new BoolValue(lhs.intValue != rhs.intValue)
      case(LE) => new BoolValue(lhs.intValue <= rhs.intValue)
      case(LT) => new BoolValue(lhs.intValue < rhs.intValue)
      case(GE) => new BoolValue(lhs.intValue >= rhs.intValue)
      case(GT) =>  new BoolValue(lhs.intValue > rhs.intValue)
      case(Plus) =>  new IntValue(lhs.intValue + rhs.intValue)  
      case(Mod) =>  new IntValue(lhs.intValue % rhs.intValue)
      case(Minus) =>  new IntValue(lhs.intValue - rhs.intValue)
      case(Times) => new IntValue(lhs.intValue * rhs.intValue)
      case(Div) => new IntValue(lhs.intValue / rhs.intValue)
    }
}
case class UnOp(op: Op1, expr: Expr) extends Expr {
  def render(indent: String): String = {
    var result = indent + "UnOp " + op + "\n"
    result = result + expr.render(indent + "  ")
    result
  }
  def interpret(t: SymbolTable):Value = {
    val v = expr.interpret(t)
    switchOp(op, v)
  }

  def switchOp(op: Op1, v: Value):Value = op match {
      case(Neg) =>  new IntValue(-1 * (v.intValue))
      case(Not) =>  new BoolValue(!(v.boolValue))
    }
}
case class Num(value: Int) extends Expr {
  def render(indent: String): String = {
    indent + "Num " + value + "\n"
  }
  def interpret(t: SymbolTable):Value = {
    val v = new IntValue(value)
    v
    }
}
case class Id(id: String) extends Expr {
  def render(indent: String): String = {
    indent + "Id " + id + "\n"
  }
  def interpret(t:SymbolTable):Value = {
    t.lookup(id)
  }
}
case object True extends Expr {
  def render(indent: String): String = {
    "True"
  }
  def interpret(t: SymbolTable):Value = {
    new BoolValue(true)
    }
}
case object False extends Expr {
  def render(indent: String): String = {
    "False"
  }
  def interpret(t: SymbolTable):Value = {
    new BoolValue(false)
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