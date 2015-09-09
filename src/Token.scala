

/**
 * @author owenunderwood_2016
 */
class Token(tokenType: String, line: Int, column: Int, lexeme: String) {
    def getLine: Int = {
      line
    }
    def getColumn: Int = {
      column
    }
    def getLexeme: String = {
      lexeme
    }
    def getType: String = {
      tokenType
    }
    override def toString: String = {
      this.tokenType + " " + this.line + ":" + this.column + " " + this.lexeme
    }
}