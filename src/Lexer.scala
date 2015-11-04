import java.io.Reader
/**
 * @author owenunderwood_2016
 */
class Lexer(in: Reader) {
    var source = new Source(in)
    
    def next: Token = {
      def state = new State
      state.tokenBuffer(source.current.toString(), source)
    }
}