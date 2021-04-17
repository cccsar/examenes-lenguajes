import java.io.File
import scala.io.StdIn

object Count_files{ 

  var counter : Int = 0

  // Extension to Java's Thread class to support Java model for concurrency
  class MyThread (var path : File) extends Thread { 
    override def run () { 
      traverse(path)
    }
  }

  def main(args: Array[String]) { 

    print ("Give me a directory: ") ;
    val path : String = readLine() 
    val root : File = new File(path)

    if (root.exists && root.isDirectory) { 
      traverse(root) 
      println(s"Number of files found under ${path}: ${counter}") 
    }
    else { 
      println(s"No such a directory") 
    }
  
  }

  // Method where counting happens
  def traverse(path : File) : Unit = { 
    val local_count : Int = path.listFiles.filter(_.isFile).length

    // protected code where each thread increases the counter for number of files 
    // found under a directory
    synchronized { 
      counter += local_count
    }

    // loop where threads are spawned 
    for (dir <- path.listFiles.filter(_.isDirectory)) { 
      val name : String = dir.getName() 
      var threads : List[MyThread] = List ()

      if ( name != "." && name != ".." ) { 
        var a_thread = new MyThread( dir ) 
        threads = a_thread :: threads
        a_thread.start
      }

      // Wait for spawned threads termination
      for (t <- threads ) {
        t.join()
      }
    }

  }

}
