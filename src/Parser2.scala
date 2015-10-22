import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Stack
import scala.collection.mutable.MutableList

/**
 * @author owenunderwood
 */
class Parser2 {
  var tokens = new ListBuffer[Token]
  var curr = 0

  var IDs = collection.mutable.Map[String, String]()

  val stmtStart = List("ID", "BEGIN", "IF", "WHILE", "PROMPT", "PRINT")
  val relOps = List("EQUAL", "NOTEQUAL", "LESSEQUAL", "GREATEREQUAL", "LESS", "GREATER")
  val mulOps = List("STAR", "DIV", "MOD", "AND")
  val unOps = List("NOT", "MINUS")

  val precedence = Map[String, Int](
    "PRINT" -> 0,
    "PLUS" -> 1,
    "MINUS" -> 1,
    "STAR" -> 2,
    "DIV" -> 2,
    "MOD" -> 2)

  val toSymbol = Map[String, String](
    "PRINT" -> "PRINT",
    "PLUS" -> "+",
    "MINUS" -> "-",
    "STAR" -> "*",
    "DIV" -> "DIV",
    "MOD" -> "MOD")

  def addToken(t: Token) {
    tokens += t
  }

  def check(tokenType: String): Boolean = {
    if (tokens(curr).getType == tokenType) {
      true
    } else {
      false
    }
  }

  def skip {
    curr += 1
  }

  def matchInput(tokenType: String): Token = {
    if (check(tokenType)) {
      val tok = tokens(curr)
      skip
      tok
    } else {
      println("Error: Expected token type: " + tokenType)
      println("Found: " + tokens(curr).getType)
      println(tokens(curr).getLine + " " + tokens(curr).getColumn)
      //sys.exit
      null
    }
  }

  def parseProgram: Program = {
    matchInput("PROGRAM")
    val a = matchInput("ID").getLexeme
    matchInput("SEMI")
    val b = parseBlock
    matchInput("PERIOD")
    val P = new Program(a, b)
    println(P)
    P
  }

  def parseBlock: Block = {
    val a = parseConstDecls
    val b = parseVarDecls
    val c = parseProcDecls
    matchInput("BEGIN")
    val d = parseStmts
    matchInput("END")
    val B = new Block(a, b, c, d)
    //println(B)
    B
  }

  def parseConstDecls: List[ConstDecl] = {
    var consts = collection.mutable.MutableList[ConstDecl]()
    while (tokens(curr).getType == "CONST") {
      consts += (parseConstDecl)
    }
    consts.toList
  }

  def parseConstDecl: ConstDecl = {
    matchInput("CONST")
    val a = matchInput("ID").getLexeme
    matchInput("ASSIGN")
    parseSign
    val b = matchInput("NUM").getLexeme.toInt
    matchInput("SEMI")
    val C = new ConstDecl(a, b)
    //println(C)
    C
  }

  def parseSign: Op1 = {
    if (tokens(curr).getType == "MINUS") {
      matchInput("MINUS")
      ////println(Neg)
      Neg
    } else
      null
  }

  def parseVarDecls: List[VarDecl] = {
    var vars = collection.mutable.MutableList[VarDecl]()
    while (tokens(curr).getType == "VAR") {
      vars += (parseVarDecl)
    }
    vars.toList
  }

  def parseVarDecl: VarDecl = {
    matchInput("VAR")
    val a = matchInput("ID").getLexeme
    matchInput("COLON")
    val b = parseType
    matchInput("SEMI")
    val V = new VarDecl(a, b)
    //println(V)
    V
  }

  def parseType: Type = {
    if (tokens(curr).getType == "INT") {
      matchInput("INT")
      IntType
    } else {
      matchInput("BOOL")
      BoolType
    }
  }

  def parseProcDecls: List[ProcDecl] = {
    var procs = collection.mutable.MutableList[ProcDecl]()
    while (tokens(curr).getType == "PROC") {
      procs += (parseProcDecl)
    }
    procs.toList
  }

  def parseProcDecl: ProcDecl = {
    matchInput("PROC")
    val a = matchInput("ID").getLexeme
    val b = parseParamList
    matchInput("SEMI")
    val c = parseBlock
    matchInput("SEMI")
    val P = new ProcDecl(a, b.getOrElse(null), c)
    //println(P)
    P
  }

  def parseParamList: Option[List[Param]] = {
    if (tokens(curr).getType == "LPAREN") {
      matchInput("LPAREN")
      val a = parseParams
      matchInput("RPAREN")
      matchInput("SEMI")
      Some(a)
    } else {
      None
    }
  }

  def parseParams: List[Param] = {
    var params = collection.mutable.MutableList[Param]()
    while (tokens(curr).getType != "PROC") {
      params += (parseParam)
    }
    params.toList
  }

  def parseParam: Param = {
    if (tokens(curr).getType == "ID") {
      val a = matchInput("ID").getLexeme
      matchInput("COLON")
      val b = parseType
      val V = new ValParam(a, b)
      //println(V)
      V
    } else {
      matchInput("VAR")
      val a = matchInput("ID").getLexeme
      matchInput("COLON")
      val b = parseType
      val V = new VarParam(a, b)
      //println(V)
      V
    }
  }

  def parseStmts: List[Stmt] = {
    var stmts = collection.mutable.MutableList[Stmt]()
    while (stmtStart.contains(tokens(curr))) {
      stmts += (parseStmt)
    }
    stmts.toList
  }

  def parseStmt: Stmt = {
    if (tokens(curr).getType == "ID") {
      parseFirstId
    } else if (tokens(curr).getType == "BEGIN") {
      matchInput("BEGIN")
      val a = parseStmts
      matchInput("END")
      matchInput("SEMI")
      val S = Sequence(a)
      println(S)
      S
    } else if (tokens(curr).getType == "IF") {
      parseFirstIf
    } else if (tokens(curr).getType == "WHILE") {
      matchInput("WHILE")
      val a = parseExpr
      matchInput("DO")
      val b = parseStmt
      val W = new While(a, b)
      //println(W)
      W
    } else if (tokens(curr).getType == "PROMPT") {
      parseFirstPrompt
    } else {
      matchInput("PRINT")
      val a = parseItems
      val P = new Print(a)
      matchInput("SEMI")
      //println(P)
      P
    }
  }

  def parseFirstId: Stmt = {
    val a = matchInput("ID").getLexeme
    if (tokens(curr).getType == "ASSIGN") {
      matchInput("ASSIGN")
      val b = parseExpr
      matchInput("SEMI")
      val A = new Assign(a, b)
      //println(A)
      A
    } else {
      val b = parseArgList
      matchInput("SEMI")
      val C = new Call(a, b)
      //println(C)
      C
    }
  }

  def parseArgList: List[Expr] = {
    if (tokens(curr).getType =="LPAREN") {
      matchInput("LPAREN")
      val a = parseExprs
      matchInput("RPAREN")
      a
    }
    else {
      null
    }
  }

  def parseFirstIf: Stmt = {
    matchInput("IF")
    val a = parseExpr
    matchInput("THEN")
    val b = parseStmt
    if (tokens(curr).getType == "ELSE") {
      matchInput("ELSE")
      val c = parseStmt
      val I = new IfThenElse(a, b, c)
      //println(I)
      I
    } else {
      val I = new IfThen(a, b)
      //println(I)
      I
    }
  }

  def parseFirstPrompt: Stmt = {
    matchInput("PROMPT")
    val a = matchInput("STRING").getLexeme
    if (tokens(curr).getType == "COMMA") {
      matchInput("COMMA")
      val b = matchInput("ID").getLexeme
      val P = new Prompt2(a, b)
      //println(P)
      P
    } else {
      val P = new Prompt(a)
      //println(P)
      (P)
    }
  }

  def parseExprs: List[Expr] = {
    var exprs = collection.mutable.MutableList[Expr]()
    while (tokens(curr).getType == "") {
      exprs += (parseExpr)
    }
    exprs.toList
  }

  def parseItems: List[Item] = {
    var items = collection.mutable.MutableList[Item]()
    while (tokens(curr).getType == "") {
      items += (parseItem)
    }
    items.toList
  }

  def parseExpr: Expr = {
    parseFirstExpr
  }

  def parseFirstExpr: Expr = {
    val a = parseSimpleExpr
    if (relOps.contains(tokens(curr))) {
      val b = parseRelOp
      val c = parseSimpleExpr
      val B = new BinOp(a, b, c)
      //println(B)
      B
    } else {
      a
    }
  }

  def parseSimpleExpr: Expr = {
    parseTerm
    parseSimpleExprPrime
  }

  def parseSimpleExprPrime: Expr = {
    parseAddOp
    parseTerm
  }

  def parseTerm: Expr = {
    parseFactor
    parseTermPrime
  }

  def parseTermPrime: Expr = {
    parseMulOp
    parseFactor
  }

  def parseItem: Item = {
    if (tokens(curr).getType == "STRING") {
      val a = matchInput("STRING").getLexeme
      val S = new StringItem(a)
      //println(S)
      S
    } else {
      val a = parseExpr
      val E = new ExprItem(a)
      //println(E)
      E
    }
  }

  def parseFactor: Expr = {
    if (tokens(curr).getType == "NUM") {
      val a = matchInput("NUM").getLexeme
      val N = new Num(a.toInt)
      //println(N)
      N
    } else if (tokens(curr).getType == "ID") {
      val a = matchInput("ID").getLexeme
      val I = new Id(a)
      //println(I)
      I
    } else if (tokens(curr).getType == "TRUE") {
      matchInput("TRUE").getLexeme
      //println(True)
      True
    } else if (tokens(curr).getType == "FALSE") {
      matchInput("TRUE").getLexeme
      //println(False)
      False
    } else if (tokens(curr).getType == "LPAREN") {
      matchInput("LPAREN")
      val a = parseExpr
      matchInput("RPAREN")
      a
    } else if (tokens(curr).getType == "MINUS") {
      matchInput("MINUS")
      val a = parseFactor
      a
    } else if (tokens(curr).getType == "NOT") {
      matchInput("NOT")
      val a = parseFactor
      a
    } else {
      //println("Invalid Factor")
      null
    }
  }

  def parseAddOp: Op2 = {
    if (tokens(curr).getType == "PLUS") {
      matchInput("PLUS")
      //println(Plus)
      Plus
    } else if (tokens(curr).getType == "MINUS") {
      matchInput("MINUS")
      //println(Minus)
      Minus
    } else if (tokens(curr).getType == "OR") {
      matchInput("OR")
      //println(Or)
      Or
    } else {
      //println("Inlvalid Add Op")
      null
    }
  }

  def parseMulOp: Op2 = {
    if (tokens(curr).getType == "STAR") {
      matchInput("STAR")
      //println(Times)
      Times
    } else if (tokens(curr).getType == "DIV") {
      matchInput("DIV")
      //println(Div)
      Div
    } else if (tokens(curr).getType == "MOD") {
      matchInput("MOD")
      //println(Mod)
      Mod
    } else if (tokens(curr).getType == "AND") {
      matchInput("AND")
      //println(And)
      And
    } else {
      //println("Inlvalid Add Op")
      null
    }
  }

  def parseRelOp: Op2 = {
    if (tokens(curr).getType == "EQUAL") {
      matchInput("EQUAL")
      //println(EQ)
      EQ
    } else if (tokens(curr).getType == "NOTEQUAL") {
      matchInput("NOTEQUAL")
      //println(NE)
      NE
    } else if (tokens(curr).getType == "LESSEQUAL") {
      matchInput("LESSEQUAL")
      //println(LE)
      LE
    } else if (tokens(curr).getType == "GREATEREQUAL") {
      matchInput("GREATEREQUAL")
      //println(GE)
      GE
    } else if (tokens(curr).getType == "LESS") {
      matchInput("LESS")
      //println(LT)
      LT
    } else if (tokens(curr).getType == "GREATER") {
      matchInput("GREATER")
      //println(GT)
      GT
    } else {
      //println("Invalid Relative Operator")
      null
    }
  }

}

