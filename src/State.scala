

/**
 * @author owenunderwood_2016
 */
class State {
  val SEMI = (";").r
  val BR = ("\\{[_]*\\}").r
  val SS = "//".r
  val WS = "\\s".r
  val PER = ".".r
  val NL = "\n".r

  val MULT = "\\*".r
  val DIV = "/".r
  val PLUS = "\\+".r
  val MINUS = "-".r
  val EQUALS = "=".r

  val PRG = "program".r
  val CONST = "const".r
  val BEG = "begin".r
  val PRNT = "print".r

  val ALPHA = "\\w*".r
  val NUM = "\\d*".r
  val ZERO = "0".r

  val ID = "\\w(\\w|[0-9])*".r

  //takes a single input character and continues to create a token as long as it matches 
  //a regular expression from above
  def tokenBuffer(lexeme: String, source: Source): Token = lexeme match {
    case SEMI() =>
      def token = new Token("SEMI", source.line, source.column, ";")
      source.advance
      token
    case BR(lexeme) =>
      def token = new Token("BR", source.line, source.column, "{}")
      source.advance
      token
    case SS(lexeme) =>
      def token = new Token("SS", source.line, source.column, "//")
      source.advance
      token
    case WS(lexeme) =>
      def token = new Token("WS", source.line, source.column, " ")
      source.advance
      token
    case PER(lexeme) =>
      def token = new Token("PER", source.line, source.column, ".")
      source.advance
      token
    case NL(lexeme) =>
      def token = new Token("NL", source.line, source.column, "\n")
      source.advance
      token
      
    case MULT(lexeme) =>
      def token = new Token("MULT", source.line, source.column, "*")
      source.advance
      token

    case DIV(lexeme) =>
      def token = new Token("DIV", source.line, source.column, "/")
      source.advance
      token
    case PLUS(lexeme) =>
      def token = new Token("PLUS", source.line, source.column, "+")
      source.advance
      token
    case MINUS(lexeme) =>
      def token = new Token("MINUS", source.line, source.column, "-")
      source.advance
      token
    case EQUALS(lexeme) =>
      def token = new Token("ASSIGN", source.line, source.column, "=")
      source.advance
      token
    case ZERO(lexeme) =>
      def token = new Token("ZERO", source.line, source.column, "0")
      source.advance
      token
    case ID(lexeme) =>
      source.advance
      tokenBuffer((lexeme + source.current.toString()), source)
    case _ => 
      def token = new Token(lexeme, source.line, source.column, lexeme)
      source.advance
      token
  }
  
  

  //  val states = Map(
  //  "Initial" -> "INIT",
  // "Operation" -> "OP",
  // "Num" -> "NUM",
  // "Identifier" -> "ID",
  // "Brace" -> "BR",
  // "Slash" -> "S",
  // "Slash Slash" -> "SS",
  // "White Space " -> "WS",
  // "Zero" -> "0",
  // "Final" -> "FINAL")
}

