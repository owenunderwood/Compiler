import java.io.InputStreamReader
import java.io.FileReader
import java.io.BufferedReader
import scala.collection.mutable.ListBuffer

/**
 * @author owenunderwood_2016
 */
object Main {
  def main(args: Array[String]) {
    val scanner = new Scanner(new BufferedReader(new InputStreamReader(System.in)))
    //val scanner = new Scanner(new BufferedReader(new FileReader("testCode.txt")))
    val parser = new Parser
     do {
      def token = scanner.next
      //println(token)
      parser.addToken(token)
    } while (scanner.source.atEOF == false)
      parser.parse
  }
}

