/**
 * @author owenunderwood_2016
 */
class State {
  val SINGLESLASH = "/"
  val SPACE = " "
  val NEWLINE = "\n"
  val WHITESPACE = """(?:[\s]|[\n]|[\t])*""".r
  val BRACKET = "{"
  val ZERO = "0"
  val ATEOF = "$"
  
  
  val SEMI = ";"
  val COLON = ":"
  val LPAREN = "("
  val RPAREN = ")"
  val COMMA = ","
  val PERIOD = "."
  
  val ASSIGN = "="
  val PLUS ="+"
  val MINUS = "-"
  val GREATER = ">"
  val LESS = "<"
  val GREATEREQUAL = ">="
  val LESSEQUAL = "<="
  val EQUAL = "=="
  val NOTEQUAL = "<>"
  val STAR = "*"
  val STRING = """""""
  
  val OP = "(\\*|\\+|-|=|==|<>|<=|>=|<|>)".r
  val NUMBER = """([1-9](?:[0-9])*)""".r
  val ID = """([A-Za-z](?:[A-Za-z0-9])*)""".r
  
  val matchPunctuation = Map[String,String] (":" -> "COLON", "(" -> "LPAREN", ")" -> "RPAREN", "," -> "COMMA", ";" -> "SEMI", "." -> "PERIOD")
  val keywords = Map[String,String] ("mod" -> "MOD", "div" -> "DIV", "program" -> "PROGRAM", "const" -> "CONST", "begin" -> "BEGIN", "print" -> "PRINT", "end" -> "END", "var" -> "VAR", "int" -> "INT", "bool" -> "BOOL", "proc" -> "PROC", "if" -> "IF", "then" -> "THEN", "else" -> "ELSE", "while" -> "WHILE", "do" -> "DO", "prompt" -> "PROMPT", "and" -> "AND", "or" -> "OR", "not" -> "NOT", "true" -> "TRUE", "false" -> "FALSE")
  val matchOp = Map[String,String] ("+" -> "PLUS", "-" -> "MINUS", "*" -> "STAR", "=" -> "ASSIGN", "==" -> "EQUAL", "<>" -> "NOTEQUAL", "<=" -> "LESSEQUAL", ">=" -> "GREATEREQUAL", ">" -> "GREATER", "<" -> "LESS")
  
  //takes a single input character and continues to create a token as long as it matches 
  //a regular expression from above
  
  def tokenBuffer(lexeme: String, source: Source): Token = lexeme match {
    case STRING =>
      def token = new Token("STRING", source.line, source.column, waitFor2(source))
      source.advance
      token 
    case ATEOF => 
      source.atEOF= true
      def token = new Token("atEOF", source.line, source.column, null)
      sys.exit  
      token 
    case BRACKET =>
      waitFor('}', source)
      source.advance
      tokenBuffer(source.current.toString(), source)
    case SINGLESLASH =>
      source.advance
      if (source.current.toString() == "/") {
        waitFor('\n', source)
        source.advance
      } else {
        print("illegal start of comment")
      }
      tokenBuffer(source.current.toString(), source)
    case SPACE =>
      if (!source.atEOF) {
        source.advance
        tokenBuffer(source.current.toString, source)
      }
      else {
        def token = new Token("EOF", source.line, source.column, null)
        token
      }
    case NEWLINE =>
      if (!source.atEOF) {
         source.advance
         tokenBuffer(source.current.toString, source)
      } else {
        def token = new Token("EOF", source.line, source.column, null)
        token
      }      
    case ZERO =>
      def token = new Token("NUM", source.line, source.column, lexeme)
      source.advance
      token
    case SEMI =>
      def token = new Token(matchPunctuation(lexeme), source.line, source.column, lexeme)
      source.advance
      token
    case COLON =>
      def token = new Token(matchPunctuation(lexeme), source.line, source.column, lexeme)
      source.advance
      token
    case LPAREN =>
      def token = new Token(matchPunctuation(lexeme), source.line, source.column, lexeme)
      source.advance
      token
    case RPAREN =>
      def token = new Token(matchPunctuation(lexeme), source.line, source.column, lexeme)
      source.advance
      token
    case PERIOD =>
      def token = new Token(matchPunctuation(lexeme), source.line, source.column, lexeme)
      source.advance
      token
    case COMMA =>
      def token = new Token(matchPunctuation(lexeme), source.line, source.column, lexeme)
      source.advance
      token
      
    // KEYWORDS & IDENTIFIERS & NUMBERS
    case WHITESPACE(lexeme) =>
      source.advance
      tokenBuffer(source.current.toString(), source)
    case ID(lexeme) =>
      source.advance
      val curr = source.current.toString()
      tokenBuffer(lexeme + curr, source)
    case NUMBER(lexeme) =>
      source.advance
      val curr = source.current.toString()
      tokenBuffer(lexeme + curr, source)
    case OP(lexeme) =>
      source.advance
      val curr = source.current.toString()
      tokenBuffer(lexeme + curr, source) 
      

    case _ =>
      var lex = lexeme.dropRight(1)
      if (keywords.contains(lex)) {
        def token = new Token(keywords.apply(lex), source.line, source.column, null)
        token
      } else if (NUMBER.pattern.matcher(lex).matches) {
        def token = new Token("NUM", source.line, source.column, lex)
        token
      } else if (OP.pattern.matcher(lex).matches) {
        def token = new Token(matchOp.apply(lex), source.line, source.column, null)
        token
      } else if (ID.pattern.matcher(lex).matches) {
        def token = new Token("ID", source.line, source.column, lex)
        token
      } else {
        println("Unexpected Character At " + source.line.toString() + ":" + source.column.toString())
        source.advance
        tokenBuffer(source.current.toString(), source)
        null
      }
  }
  def doubleQuotes(source: Source): Boolean = {
    var b = false
    if (source.current == '"') {
      source.advance
      if (source.current == '"') {
         b = true
        }
    }
    b
  }
  
  def waitFor2(source: Source): String = {
    var s = '"'.toString
    while (source.current != '"') {
        s = s+source.current
        source.advance
      }
    s = s+source.current
    source.advance
    s
    }      


  def waitFor(ch: Char, source: Source) {
    while(source.current != ch) {
      if (source.atEOF) {
        println("Unclosed comment")
      }
      else {
        source.advance
      }
    }      
  }
}
