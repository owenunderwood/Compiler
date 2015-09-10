

/**
 * @author owenunderwood_2016
 */
class State {
  val SEMI = ";"
  val BRACKET = "{"
  val SINGLESLASH = "/"
  val WHITESPACE = """\s""".r
  val PERIOD = "."
  val NEWLINE = """\n""".r

  val STAR = "*"
  val DIV = "/"
  val PLUS = "+"
  val MINUS = "-"
  val EQUALS = "="

  val PRG = "program"
  val CONST = "const"
  val BEG = "begin"
  val PRNT = "print"

  val ZERO = "0"

  val NUMBER = """[1-9]?:([0-9])*""".r
  val ID = """(\w?:(\w|\d)*)""".r

  //takes a single input character and continues to create a token as long as it matches 
  //a regular expression from above
  def tokenBuffer(lexeme: String, source: Source): Token = lexeme match {

    //PUNCTUATION & WHITESPACES
    case SEMI =>
      def token = new Token("SEMI", source.line, source.column, null)
      source.advance
      token
    case BRACKET =>
      waitFor('}', source)
      tokenBuffer("", source)
    case SINGLESLASH =>
      source.advance
      if (source.current.toString() == "/") {
        waitFor('\n', source)
      } else {
        print("illegal start of comment")
      }
      tokenBuffer("", source)
    case WHITESPACE(lexeme) =>
      source.advance
      tokenBuffer(source.current.toString(), source)
    case NEWLINE(lexeme) =>
      source.advance
      tokenBuffer(source.current.toString(), source)
    case PERIOD =>
      def token = new Token("PERIOD", source.line, source.column, null)
      source.advance
      token

    //OPERATORS      
    case STAR =>
      def token = new Token("MULT", source.line, source.column, null)
      source.advance
      token
    case DIV =>
      def token = new Token("DIV", source.line, source.column, null)
      source.advance
      token
    case PLUS =>
      def token = new Token("PLUS", source.line, source.column, null)
      source.advance
      token
    case MINUS =>
      def token = new Token("MINUS", source.line, source.column, null)
      source.advance
      token
    case EQUALS =>
      def token = new Token("ASSIGN", source.line, source.column, null)
      source.advance
      token
    case ZERO =>
      def token = new Token("ZERO", source.line, source.column, null)
      source.advance
      token
      
      // KEYWORDS & IDENTIFIERS & NUMBERS
    case ID(lexeme) =>
      val curr = source.current.toString()
      source.advance
      tokenBuffer(lexeme + curr, source)
    case _ =>
      if (lexeme.equals(PRG)) {
        def token = new Token("PROGRAM", source.line, source.column, null)
        source.advance
        token
      } else if (lexeme.equals(CONST)) {
        def token = new Token("CONST", source.line, source.column, null)
        source.advance
        token
      } else if (lexeme.equals(BEG)) {
        def token = new Token("BEG", source.line, source.column, null)
        source.advance
        token
      } else if (lexeme.equals(PRNT)) {
        def token = new Token("PRINT", source.line, source.column, null)
        source.advance
        token
      } else if (NUMBER.pattern.matcher(lexeme).matches) {
        def token = new Token("NUM", source.line, source.column, lexeme)
        source.advance
        token
      } else {
        def token = new Token("ID", source.line, source.column, lexeme)
        source.advance
        token
      }
  }

  def waitFor(ch: Char, source: Source) {
    while (source.current != ch) {
      source.advance
    }
  }
  
  
}

