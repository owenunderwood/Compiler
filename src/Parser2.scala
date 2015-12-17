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
  val addOps = List("PLUS", "MINUS", "OR")
  val mulOps = List("STAR", "DIV", "MOD", "AND")

  val binOps = List("PLUS", "STAR", "DIV", "MINUS", "OR", "AND", "GREATER", "LESS", "MOD", "GREATEREQUAL", "LESSEQUAL", "NOTEQUAL", "EQUAL")
  val matchFactor = List("NUM", "ID", "TRUE", "FALSE", "MINUS", "NOT", "LPAREN")
  val matchItem = List("NUM", "ID", "TRUE", "FALSE", "MINUS", "NOT", "LPAREN", "STRING")

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
    if (tokens(curr).getType == "CONST") {
      while (tokens(curr).getType == "CONST") {
        consts += (parseConstDecl)
      }
      consts.toList
    } else {
      Nil
    }
  }

  def parseConstDecl: ConstDecl = {
    matchInput("CONST")
    val a = matchInput("ID").getLexeme
    matchInput("ASSIGN")
    val b = parseSign.toInt * matchInput("NUM").getLexeme.toInt
    matchInput("SEMI")
    val C = new ConstDecl(a, b)
    //println(C)
    C
  }

  def parseSign: Int = {
    if (tokens(curr).getType == "MINUS") {
      matchInput("MINUS")
      ////println(Neg)
      -1
    } else
      1
  }

  def parseVarDecls: List[VarDecl] = {
    var vars = collection.mutable.MutableList[VarDecl]()
    if (tokens(curr).getType == "VAR") {
      while (tokens(curr).getType == "VAR") {
        vars += (parseVarDecl)
      }
      vars.toList
    } else {
      Nil
    }
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
    if (tokens(curr).getType == "PROC") {
      while (tokens(curr).getType == "PROC") {
        procs += (parseProcDecl)
      }
      procs.toList
    } else {
      Nil
    }
  }

  def parseProcDecl: ProcDecl = {
    matchInput("PROC")
    val a = matchInput("ID").getLexeme
    val b = parseParamList
    matchInput("SEMI")
    val c = parseBlock
    matchInput("SEMI")
    val P = new ProcDecl(a, b, c)
    //println(P)
    P
  }

  def parseParamList: List[Param] = {
    if (tokens(curr).getType == "LPAREN") {
      matchInput("LPAREN")
      val a = parseParams
      matchInput("RPAREN")
      a
    } else {
      Nil
    }
  }

  def parseParams: List[Param] = {
    var params = collection.mutable.MutableList[Param]()
      while (tokens(curr).getType != "RPAREN") {
        params += (parseParam)
      }
      params.toList
  }

  def parseParam: Param = {
    if (tokens(curr).getType == "ID") {
      val a = matchInput("ID").getLexeme
      matchInput("COLON")
      val b = parseType
      if (tokens(curr).getType == "COMMA") {
        matchInput("COMMA")
      }
      val V = new ValParam(a, b)
      //println(V)
      V
    } else {
      matchInput("VAR")
      val a = matchInput("ID").getLexeme
      matchInput("COLON")
      val b = parseType
      if (tokens(curr).getType == "COMMA") {
        matchInput("COMMA")
      }
      val V = new VarParam(a, b)
      //println(V)
      V
    }
  }

  def parseStmts: List[Stmt] = {
    var stmts = collection.mutable.MutableList[Stmt]()
    if (stmtStart.contains(tokens(curr).getType)) {
      while (stmtStart.contains(tokens(curr).getType)) {
        stmts += (parseStmt)
      }
      stmts.toList
    } else {
      Nil
    }
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
      //println(S)
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
      matchInput("SEMI")
      val P = new Print(a)
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
      matchInput("SEMI")
      //println(P)
      P
    } else {
      val P = new Prompt(a)
      matchInput("SEMI")
      //println(P)
      (P)
    }
  }

  def parseArgList: List[Expr] = {
    if (tokens(curr).getType == "LPAREN") {
      matchInput("LPAREN")
      val a = parseArgs
      matchInput("RPAREN")
      a
    } else {
      Nil
    }
  }

  def parseArgs: List[Expr] = {
    var args = collection.mutable.MutableList[Expr]()
    if (matchFactor.contains(tokens(curr).getType)) {
      while (matchFactor.contains(tokens(curr).getType)) {
        args += (parseArg)
      }
      args.toList
    } else {
      Nil
    }
  }

  def parseArg: Expr = {
    val a = parseExpr
    if (tokens(curr).getType == "COMMA") {
      matchInput("COMMA")
    }
    a
  }

  def parseExprs: List[Expr] = {
    var exprs = collection.mutable.MutableList[Expr]()
    if (matchFactor.contains(tokens(curr).getType)) {
      while (matchFactor.contains(tokens(curr).getType)) {
        exprs += (parseExpr)
      }
      exprs.toList
    } else {
      Nil
    }
  }

  def parseItems: List[Item] = {
    var items = collection.mutable.MutableList[Item]()
    if (matchItem.contains(tokens(curr).getType)) {
    while (matchItem.contains(tokens(curr).getType)) {
      items += (parseItem)
    }
    items.toList
    }
    else {
      Nil
    }
    
  }

  def parseItem: Item = {
    if (tokens(curr).getType == "STRING") {
      val a = matchInput("STRING").getLexeme
      val S = new StringItem(a)
      if (tokens(curr).getType == "COMMA") {
        matchInput("COMMA")
      }
      //println(S)
      S
    } else {
      val a = parseExpr
      val E = new ExprItem(a)
      if (tokens(curr).getType == "COMMA") {
        matchInput("COMMA")
      }
      //println(E)
      E
    }
  }

  // <Expr> -->
  // <SimpleExpr> <RelOp> <SimpleExpr>
  // | <SimpleExpr>

  def parseExpr: Expr = {
    val a = parseSimpleExpr
    if (relOps.contains(tokens(curr).getType)) {
      val b = parseRelOp
      val c = parseSimpleExpr
      val B = BinOp(a, b, c)
      B
    } else {
      a
    }
  }

  // <SimpleExpr> -->
  // <SimpleExpr> <AddOp> <Term>
  // | <Term>

  def parseSimpleExpr: Expr = {
    val a = parseTerm
    if (addOps.contains(tokens(curr).getType)) {
      val b = parseAddOp
      val c = parseTerm
      val B = BinOp(a, b, c)
      B
    } else {
      a
    }
  }

  def parseTerm: Expr = {
    val a = parseFactor
    if (mulOps.contains(tokens(curr).getType)) {
      val b = parseMulOp
      val c = parseFactor
      val B = BinOp(a, b, c)
      B
    } else {
      a
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
      matchInput("FALSE").getLexeme
      //println(False)
      False
    } else if (tokens(curr).getType == "LPAREN") {
      matchInput("LPAREN")
      val a = parseExpr
      matchInput("RPAREN")
      a
    } else if (tokens(curr).getType == "MINUS") {
      val a = matchInput("MINUS")
      val b = parseExpr
      UnOp(Neg, b)
    } else {
      val a = matchInput("NOT")
      val b = parseExpr
      UnOp(Not, b)
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
    } else {
      matchInput("OR")
      //println(Or)
      Or
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
    } else {
      matchInput("AND")
      //println(And)
      And
    }
  }

  // <RelOp> -->
  //== | <> | <= | >= | < | >

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
    } else {
      matchInput("GREATER")
      //println(GT)
      GT
    }
  }
  def parseOp1: Op1 = {
    if (tokens(curr).getType == "MINUS") {
      Neg
    } else {
      Not
    }
  }

}

