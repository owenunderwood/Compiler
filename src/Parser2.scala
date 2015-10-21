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
  val relOps = List ("EQUAL", "NOTEQUAL", "LESSEQUAL", "GREATEREQUAL", "LESS", "GREATER")
  val mulOps = List ("STAR", "DIV", "MOD", "AND")
  val unOps = List ("NOT", "MINUS")
  
  val precedence = Map[String,Int](
                "PRINT" -> 0, 
                "PLUS" -> 1, 
                "MINUS" -> 1, 
                "STAR" -> 2, 
                "DIV" -> 2, 
                "MOD" -> 2)
                
  val toSymbol = Map[String,String](
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
    if (tokens(curr).getType==tokenType) {
      true
    }
    else {
      false
    }
  }
  
  def skip {
    curr+=1
  }
  
  def matchInput(tokenType: String): Token = {
    if (check(tokenType)) {
      val tok = tokens(curr)
      skip
      tok
    }
    else {
      println("Error: Expected token type: " + tokenType)
      println("Found: " + tokens(curr).getType)
      println(tokens(curr).getLine + " " + tokens(curr).getColumn)
      sys.exit
      null
    }
  }
  
  def parseProgram: Program = {
    matchInput("PROGRAM")
    val a = matchInput("ID").getLexeme    
    matchInput("SEMI")
    val b = parseBlock
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
    B
  }
  
  def parseConstDecls: List[ConstDecl] = {
    var consts = collection.mutable.MutableList[ConstDecl]()
    while (tokens(curr).getType == "CONST") {
      consts+= (parseConstDecl)    
    }
    consts.toList
  }
  
  def parseVarDecls: List[VarDecl] = {
    var vars = collection.mutable.MutableList[VarDecl]()
    while (tokens(curr).getType == "VAR") {
      vars += (parseVarDecl)    
    }
    vars.toList
  }
  
  def parseProcDecls: List[ProcDecl] = {
    var procs = collection.mutable.MutableList[ProcDecl]()
    while (tokens(curr).getType == "PROC") {
      procs += (parseProcDecl)    
    }
    procs.toList
  }
  
  def parseStmts: List[Stmt] = {
    var stmts = collection.mutable.MutableList[Stmt]()
    while (stmtStart.contains(tokens(curr))) {
      stmts += (parseStmt)    
    } 
    stmts.toList
  }
  
  def parseConstDecl: ConstDecl = {
    matchInput("CONST")
    val a = matchInput("ID").getLexeme
    matchInput("ASSIGN")
    parseSign
    val b = matchInput("NUM").getLexeme.toInt
    matchInput("SEMI")
    val C = new ConstDecl(a, b)
    C
  }
  
  def parseVarDecl: VarDecl = {
    matchInput("VAR")
    val a = matchInput("ID").getLexeme
    matchInput("COLON")
    val b = parseType
    matchInput("SEMI")
    val V = new VarDecl(a, b)
    V
  }
  
  def parseProcDecl: ProcDecl = {
    matchInput("PROC")
    val a = matchInput("ID").getLexeme
    val b = parseParamList
    matchInput("SEMI")
    val c = parseBlock
    matchInput("SEMI")
    val P = new ProcDecl(a, b, c)
    P
  }
  
  def parseStmt: Stmt = {
    if (tokens(curr).getType == "ID") {
      parseFirstId
    }else if (tokens(curr).getType == "BEGIN") {
      matchInput("BEGIN")
      val a = parseStmts
      matchInput("END")
      matchInput("SEMI")
      val S = new Sequence(a)
      S
    }else if (tokens(curr).getType == "IF") {
      parseFirstIf
    }else if (tokens(curr).getType == "WHILE") {
      matchInput("WHILE")
      val a = parseExpr
      matchInput("DO")
      val b = parseStmt
      val W = new While(a, b)
      W
    }else if (tokens(curr).getType == "PROMPT") {
      parseFirstPrompt
    }else {
      val a = parseItems
      val P = new Print(a)
      P
    }
  }
  
  def parseType: Type = {
    if (tokens(curr).getType == "INT") {
      IntType
    }
    else if (tokens(curr).getType == "BOOL") {
      BoolType
    }
    else
      println("Invalid Type")
      sys.exit
  }
  
  def parseSign: Op1 = {
    if (tokens(curr).getType == "MINUS") {
      matchInput("MINUS")
      Neg
    }
    else
      null
  }
  
  def parseParamList: List[Param] = {
    matchInput("LPAREN")
    val a = parseParams
    matchInput("RPAREN")
    a
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
      V
    }
    else {
      val a = matchInput("ID").getLexeme
      matchInput("COLON")
      val b = parseType
      val V = new VarParam(a, b)
      V
    }
  }
    
  def parseFirstId: Stmt = {
      val a = matchInput("ID").getLexeme
      if (tokens(curr).getType == "ASSIGN") {
        matchInput("ASSIGN")
        val b = parseExpr
        matchInput("SEMI")
        val A = new Assign(a, b)
        A
      }
      else {
        val b = parseArgList
        matchInput("SEMI")
        val C = new Call(a, b)
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
        I
      }
      else {
        val I = new IfThen(a, b)
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
        P
      }
      else {
        val P = new Prompt(a)
        P
      }
    }
    
    def parseArgList: List[Expr] = {
      matchInput("LPAREN")
      val a = parseExprs
      matchInput("RPAREN")
      a
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
        B
      }
      else {
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
        S
      }
      else {
        val a = parseExpr
        val E = new ExprItem(a)
        E
      }
    }
    
    def parseFactor: Expr = {
      if (tokens(curr).getType == "NUM") {
        val a = matchInput("NUM").getLexeme
        val N = new Num(a.toInt)
        N
      }
      else if (tokens(curr).getType == "ID") {
        val a = matchInput("ID").getLexeme
        val I = new Id(a)
        I
      }
      else if (tokens(curr).getType == "TRUE") {
        matchInput("TRUE").getLexeme
        True
      }
      else if (tokens(curr).getType == "FALSE") {
        matchInput("TRUE").getLexeme
        False
      }
      else if (tokens(curr).getType == "TRUE") {
        matchInput("TRUE").getLexeme
        True
      }
      else if (unOps.contains(tokens(curr))) {
        val a = parseUnOp
        val b = parseFactor
        val U = new UnOp(a, b)
        U
      }
      else if (tokens(curr).getType == "LPAREN") {
        matchInput("LPAREN")
        val a = parseExpr
        matchInput("RPAREN")
        a
      }
      else {
        println("Invalid Factor")
        null
      }
    }
    
    def parseUnOp: Op1 = {
      if (tokens(curr).getType == "MINUS") {
        matchInput("MINUS")
        Neg
      }
      else if (tokens(curr).getType == "NOT") {
        matchInput("NOT")
        Not
      }
      else {
        println("Invalid Unary Operator")
        null
      }
    }
    
    def parseAddOp: Op2 = {
      if (tokens(curr).getType == "PLUS") {
        matchInput("PLUS")
        Plus
      }
      else if (tokens(curr).getType == "MINUS") {
        matchInput("MINUS")
        Minus
      }
      else if (tokens(curr).getType == "OR") {
        matchInput("OR")
        Or
      }
      else {
        println("Inlvalid Add Op")
        null
      }
    }
    
    def parseMulOp: Op2 = {
    if (tokens(curr).getType == "STAR") {
        matchInput("STAR")
        Times
      }
      else if (tokens(curr).getType == "DIV") {
        matchInput("DIV")
        Div
      }
      else if (tokens(curr).getType == "MOD") {
        matchInput("MOD")
        Mod
      }
    else if (tokens(curr).getType == "AND") {
        matchInput("AND")
        And
      }
      else {
        println("Inlvalid Add Op")
        null
      }
    }
    
    def parseRelOp: Op2 = {
      if (tokens(curr).getType == "EQUAL") {
        matchInput("EQUAL")
        EQ
      }
      else if (tokens(curr).getType == "NOTEQUAL") {
        matchInput("NOTEQUAL")
        NE
      }
      else if (tokens(curr).getType == "LESSEQUAL") {
        matchInput("LESSEQUAL")
        LE
      }
      else if (tokens(curr).getType == "GREATEREQUAL") {
        matchInput("GREATEREQUAL")
        GE
      }
      else if (tokens(curr).getType == "LESS") {
        matchInput("LESS")
        LT
      }
      else if (tokens(curr).getType == "GREATER" ) {
        matchInput("GREATER")
        GT
      }
      else {
        println("Invalid Relative Operator")
        null
      }    
    }

}

