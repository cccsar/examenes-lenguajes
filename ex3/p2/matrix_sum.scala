import scala.util.control.Breaks._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

import scala.io.StdIn
import java.util.Scanner;

object Matrix_sum { 
  def main(args: Array[String]): Unit = { 

    val scanner = new Scanner(System.in) 

    // provide matrix dimensions
    var rows : Int = scanner.nextInt()
    var cols : Int = scanner.nextInt() 

    // elaborate them
    var a = Array.ofDim[Int](rows,cols)
    var b = Array.ofDim[Int](rows,cols)
    var c = Array.ofDim[Int](rows,cols) // se inicializa en 0

    // storring the result of the futures
    var res = new Array[Future[Unit]](rows)
    var guard = true

    readMatrix(a)
    readMatrix(b)
    initializeMat(c)

    // Spawn futures for each row of the matrix
    for (i<-0 to rows-1) { 
      res(i) = arrSum(a(i),b(i),c(i))
    }

    // check that none of the futures failed
    for (i<-0 to rows-1) { 
      res(i) onFailure { 
        case e => println(s"Exception ${e.getMessage}") 
        guard = false
        break
      }
    }

    // on success, print matrix
    if (guard) { 
      dbgMatrix(c)
    }
    else { 
      println (s"Error while coputing matrix")
    }
  }

  def readMatrix (mat : Array[ Array[Int]] ) = { 
    val scanner = new Scanner(System.in) // simply use scanner

    for (i<-0 to (mat.length-1)) { 
      for (j<-0 to mat(0).length-1) { 
        mat(i)(j) = scanner.nextInt()
      }
    }
  }

  def dbgMatrix ( mat : Array[ Array[Int] ] ) = { 
    for (i<-mat) { 
      for (j <- i) { 
        print (j + " ")
      }
      println ("")
    }
  }

  // Future for the sum of an array. Used to sum rows of a matrix concurrently
  def arrSum ( a : Array[ Int ], b : Array[ Int ] , res : Array[ Int] ) : Future[Unit] = Future { 
    for (i <- 0 to a.length-1) {
      res(i) = a(i) + b(i)
    }
  }

  def initializeMat ( a : Array[ Array[Int] ]) { 
    for (i<-0 to a.length -1 )  { 
      for (j<-0 to a(0).length -1 ) {
        a(i)(j) = 0
      }
    }
  }

}
