import java.io.Reader
/**
 * @author owenunderwood_2016
 */
class Source(in: Reader) {
  var line = 0
  var column = 0
  var current = '\n'
  var atEOF = false

  def Source(in: Reader) {
    this.advance
    var line = 0
    var column = 0
    var current = '\n'
    var atEOF = false
    advance
  }

  def advance {
    if (atEOF) {
      return
    } else if (current == '\n') {
      line += 1
      column = 1
    } else {
      column += 1
    }

    var next = in.read

    if (next == -1) {
      atEOF = true
    } else {
      current = next.toChar
    }
  }
}