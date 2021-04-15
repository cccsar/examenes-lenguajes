import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}


import java.util.Scanner
import java.io.File
import scala.io.StdIn

object Count_files{ 

  def main(args: Array[String]) { 

    println ("Give me a directory:") ;

    val path : String = readLine() 
//    val files : File = new File(path)
    val result  = traverse(path)
    var cond = true 

    //files.listFiles.foreach { println } 

    result.onComplete { 
      case Success(x) => {  
        println(s"Number of directories ${x}" )
        cond = false
      }
      case Failure(e) => { 
        println("aqui") 
        cond = false
      }
    }

  }

  def traverse(path : String) : Future[Int] = Future { 
    val thisD : File  = new File(path)
    var acc : Int = 0 

    acc = thisD.listFiles.filter(_.isDirectory).length 
    acc
  }

}
