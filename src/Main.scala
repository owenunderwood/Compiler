import java.io.File
import java.io.FileReader
import java.io.IOException
import java.io.PrintStream
import java.io.Reader
import java.io.BufferedReader
import  java.io.InputStreamReader

/** 
 * @author owenunderwood_2016
 */
object Main {
  def main(args: Array[String]) {
    val consoleOuptut = new PrintStream(System.out)
    val consoleInput = new Scanner(new BufferedReader(new InputStreamReader(System.in)))
     
    var source:File = null
    
    if (args.length > 0) {
      source = new File(args(0))
    } else {
      consoleOuptut.print("Source file? ")
      val sourceFilename = consoleInput.nextLine
      source = new File(sourceFilename)
    }
    
    val scanner = new Scanner(new BufferedReader(new FileReader(source)))
    val parser = new Parser2
    val P = parser.parseProgram
    
     do {
      def token = scanner.next
      parser.addToken(token)      
    } while (scanner.source.atEOF == false)
      for (token <- parser.tokens) {
        println(token)
      }
        
      val program = parser.parseProgram
      val result = program.render("")
      println(result)
  }
}

