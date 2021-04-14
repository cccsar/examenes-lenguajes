import scala.io.StdIn
import java.util.Scanner;

object Prueba { 
  def main(args: Array[String]): Unit = { 

    val scanner = new Scanner(System.in) 

    // provide matrix dimensions
    var rows : Int = scanner.nextInt()
    var cols : Int = scanner.nextInt() 

    // elaborate them
    var a = Array.ofDim[Int](rows,cols)
    var b = Array.ofDim[Int](rows,cols)
    var c = Array.ofDim[Int](rows,cols) // se inicializa en 0

    readMatrix(a)
    readMatrix(b)
    initializeMat(c)

    sumRows(a,b,c,1)

    dbgMatrix(c)
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

  // this one has to be parallel
  def sumRows ( a : Array[ Array[Int] ], b : Array[ Array[Int] ] , res : Array[ Array[Int] ] , r : Int ) = { 
    assert( a.length == b.length && a(0).length == b(0).length ) // require matrix dimentions to match


    for (i <- 0 to a(0).length-1) {
      res(r)(i) = a(r)(i) + b(r)(i)
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
