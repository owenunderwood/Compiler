

/**
 * @author owenunderwood_2016
 */
class Token(tokenType: String, line: Int, column: Int, lexeme: String) {
    def getLine: Int = {
      line
    }
    def getColumn: Int = {
      if (lexeme==null) {
        val c = column-1
        c
      }
      else {
        val c = column - lexeme.length()
        c
      }
    }
    def getLexeme: String = {
      lexeme
    }
    def getType: String = {
      tokenType
    }
    override def toString: String = {
      if (lexeme==null) {
        this.tokenType + " " + this.line + ":" + getColumn + " "
      }
      else {
        this.tokenType + " " + this.lexeme + " " + this.line + ":" + getColumn
      }
    }
}