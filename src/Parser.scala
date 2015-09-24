//<Program> -->
//  program ID ; <Block> .
//
//<Block> -->
//  <ConstDecls> begin <Stmts> end

//<ConstDecls> -->
//  <ConstDecl> <ConstDecls>
//|

//<ConstDecl> --> const ID = NUM ;

//<Stmts> -->
//  <Stmt> <Stmts>
//|

//<Stmt> -->
//  print <Expr> ;

//<Expr> -->
//  <Expr> + <Term>
//| <Expr> - <Term>
//| <Term>

//<Term> -->
//  <Term> * <Factor>
//| <Term> div <Factor>
//| <Term> mod <Factor>
//| <Factor>

//<Factor> -->
//  NUM
//| ID
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Stack

/**
 * @author owenunderwood_2016
 */
class Parser {
  var tokens = new ListBuffer[Token]
  var curr = 0

  var IDs = collection.mutable.Map[String, String]()

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
      null
    }
  }

  def parse {
    matchInput("PROGRAM")
    matchInput("ID")
    matchInput("SEMI")
    parseConstantDeclarations
    matchInput("BEGIN")
    while (check("END") == false) {
      parseStatements
    }
    matchInput("END")
    matchInput("PERIOD")
    matchInput("EOF")
  }

  def parseConstantDeclarations {
    while (check("CONST")) {
      matchInput("CONST")
      var s = matchInput("ID").getLexeme
      matchInput("ASSIGN")
      var n = matchInput("NUM").getLexeme
      IDs += (s -> n)
      matchInput("SEMI")
    }
  }

  def parseStatements {
    val s = new collection.mutable.Stack[String]()
    s.push("PRINT")
    while (!check("SEMI")) {

      if (check("PRINT")) {
        matchInput("PRINT")
      }

      if (check("NUM")) {
        println(matchInput("NUM").getLexeme)
      } else if (check("ID")) {
        if (IDs.contains(tokens(curr).getLexeme)) {
          println(IDs((matchInput("ID").getLexeme)))
        } else
          println("No identifier created")
      } else if (check("PRINT") | check("STAR") | check("MINUS") | check("PLUS") | check("DIV") | check("MOD")) {
        var op = tokens(curr).getType
        matchInput(op)
        while (precedence(s.top) >= precedence(op)) {
          println(toSymbol(s.pop))
        }
        s.push(op)
      } else {
        println("Error.  Token :" + tokens(curr).getType + " is not allowed")
        sys.exit(1)
      }
    }
    while (s.isEmpty == false) {
      println(toSymbol(s.pop))
    }
    matchInput("SEMI")
  }
}

  
