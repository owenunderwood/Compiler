import java.io.File
import java.io.FileReader
import java.io.IOException
import java.io.PrintStream
import java.io.Reader
import java.util.Scanner

/**
 * @author owenunderwood_2016
 */
object Main {
  def main(args: Array[String]) {
    val consoleOuptut = new PrintStream(System.out)
    val consoleInput = new Scanner(System.in)

    var source: File = null

    if (args.length > 0) {
      source = new File(args(0))
    } else {
      consoleOuptut.print("Source file? ")
      val sourceFilename = consoleInput.nextLine
      source = new File(sourceFilename)
    }

    val in = new FileReader(source)
    val lexer = new Lexer(in)
    val parser = new Parser2
    val P = parser.parseProgram

    in.close()
    // do {
    //  def token = lexer.next
    //  parser.addToken(token)      
    //} while (lexer.source.atEOF == false)
    //  for (token <- parser.tokens) {
    //    println(token)
    //  }

    val program = parser.parseProgram
    program.interpret
    val result = program.render("")
    println(result)
  }
}

