
import java.util.Scanner
import java.io.File
import scala.io.StdIn

object Count_files{ 

  def main(args: Array[String]) : Unit{ 

    println("Give me a directory:") 
    val scanner = new Scanner(System.in)
    val path = scanner.nextString()
    val files : File = new File(path)

    val something = files.listFiles.foreach { prinln } 

  }

}
