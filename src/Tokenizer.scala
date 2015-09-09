
/**
 * @author owenunderwood_2016
 */
class Tokenizer {
  val keywords = ("program", "const", "begin", "print")
  val IDs = ()
  val Identifier = "[A-Za-z][A-Za-z0-9]*".r
  val Number = "[1-9][0-9]*".r
  val Punctuation = ".|;".r
  val WhiteSpace = " ".r
  val Operator = "[-+*/^]".r

  def parse(source: Source): Token = {
    var current = source.current.toString()
    var lexeme = current.toString()
    val line = source.line.toInt
    val column = source.column.toInt
    while (Identifier.pattern.matcher(lexeme).matches) {
      lexeme = lexeme + current
      current = source.current.toString()
    }
    def token = new Token(identify(lexeme), line, column, lexeme)
    token
  }

  def identify(word: String): String = word match {
    case Identifier(word)  => return "Identifier";
    case Number(word)      => return "Number"
    case Punctuation(word) => return "Punctuation"
    case Operator(word)    => return "Operator"
    case _                 => return "Whitespace"
  }

}
