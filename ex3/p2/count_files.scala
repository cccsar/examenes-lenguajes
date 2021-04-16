import java.io.File
import scala.io.StdIn

class MyThread extends Thread { 
  override def run () { 
    println (s"I'm ${ Thread.currentThread().getName } ") 
  }
}

object Count_files{ 

  var counter : Int = 0

  def main(args: Array[String]) { 

    var xd = new MyThread() 
    var xp = new MyThread()

    xd.start()
    xp.start() 

    println(s"result: ${counter}") 

    //println ("Give me a directory:") ;
    //val path : String = readLine() 

    //files.listFiles.foreach { println } 
  }

  def do_something(el : Int) : Unit = { 
    synchronized { 
      counter += el  
    }
  }

}
