package edu.washington.cs.knowitall.pattern.lda
import java.util.Scanner
import java.io.FileInputStream
import java.io.File

object ConvertLDAOutput {
  val linePattern = """(\d+\s+)+\s*;"""
    
  def main(args: Array[String]) {
    val input =
      if (args.length == 0) System.in
      else new FileInputStream(new File(args(0)))
    
    try {
      val scan = new Scanner(input)

      while (scan.hasNext) {
        val token = scan.next()
        if (token == "z") {
          assert(scan.next() == "=")
          
          while (scan.hasNext) {
	        var line = List[Int]()
	        while (scan.hasNextInt) {
	          line ::= scan.nextInt()
	        }
	        
	        assert(scan.next() == ";")
	        
	        if (!line.isEmpty) {
	          println(line.reverse.mkString(" "))
	        }
	      }
        }
        else {
          scan.nextLine
        }
      }
    } finally {
      input.close
    }
  }
}