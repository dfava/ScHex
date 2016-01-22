/** Hex board game
* An implementation of the hex board game in Scala
* See https://en.wikipedia.org/wiki/Hex_(board_game) for details on the game
*
* @author Daniel Fava
* http://github.com/dfava/ScHex.git
*/
import sys.exit
import scala.math._

case class Board(size : Int) {
    var cells = Array.fill[Char](size, size)('.')
    var iteration = 0 // Incremented every time a player makes a valid move

    def play() {
        def readStdIn() : Array[Int] = {
            // Read from standard-in until it reads a line where the first two items
            // (separated by white space) are integers within 1 and size
            val line = readLine()
            val coord = try {
                for (i <- line.split("\\s+").slice(0,2)) yield i.toInt
            } catch {
                // TODO: Send CNTRL D to exit(0)
                case _ if (line == ":q") => println("Bye."); exit(0)
                case _ : Throwable => println("Try again.  Enter two numbers between 1 and " + size + "."); readStdIn()
            }
            if (coord(0) < 1 || coord(1) < 1 || coord(0) > size || coord(1) > size) {
                println("Try again.  Enter two numbers between 1 and " + size + ".")
                readStdIn()
            } else coord
        }
        var coord = readStdIn() 
        while (cells(coord(0)-1)(coord(1)-1) != '.') {
            println("Try again.  That cell is already taken.")
            coord = readStdIn()
        }
        cells(coord(0)-1)(coord(1)-1) = if (iteration % 2 == 0) '0' else 'X'
        iteration += 1
    }

    def gameOver() = { // Detect whether a player has won, or whether the board is full
        false // TODO: Implement
    }

    def cellsToLines() = { // Helper function to toString
        // Transforms cells into an array of strings, each string being a board row
        for { rowIdx <- -1 to size} yield {
            if (rowIdx == -1 || rowIdx == size) "  " + (" " * (rowIdx-1)) + ("X " * size)
            else (" " * rowIdx) + "0 " + cells(rowIdx).mkString(" ") + " 0" }
    }

    def colNumbers = { // Return a vector representing the indices of every column
        val digits = log10(size).toInt+1
        for (digit <- digits-1 to 0 by -1) yield {
            (for (i <- 0 to size-1) yield { ((i+1) / pow(10, digit).toInt) % 10 }).mkString(" ")
        }
    }

    override def toString() = {
        val board = cellsToLines
        val decoratedBoard = for {
                i <- -1 until board.length
                val spaces = size.toString.length - (i).toString.length + 1
            } yield {
                if (i == 0 || i == size+1) (" " * (spaces + i.toString.length)) + board(i)
                else if (i == -1) (" " * (spaces+4)) + colNumbers.mkString("\n" + (" " * (spaces+4)))
                else i.toString + (" " * spaces) + board(i)
            }
        decoratedBoard.mkString("\n")
    }
}

object hex {
    val default = Map('size -> "10")
    val usage = s"""usage: hex [-h] [-s SIZE]

Hex board game

optional arguments:
  -h, -help             show this help message and exit
  -s SIZE               set the board size (default: $default('size))"""
    def main(args : Array[String]) {
        type OptionMap = Map[Symbol, String]
        def parse(map : OptionMap, list : List[String]) : OptionMap = {
            def isSwitch(s : String) = (s(0) == '-')
            list match {
                case Nil => map // Base case
                case "-s" :: value :: tail if value.toInt > 0 => parse(map ++ Map('size -> value), tail)
                case option :: tail if List("-h", "-help").contains(option) => println(usage); exit(0)
                case option :: tail if List("-s").contains(option) => println("Invalid value passed to " + option); exit(1)
                case option :: tail => println("ERR: Unknown option " + option); exit(1)
            }
        }
        val options = try { parse(Map(), args.toList) } catch { case e : NumberFormatException => println("Invalid value passed to parameter"); exit(1) }
        val b = Board((default ++ options)('size).toInt)
        while (!b.gameOver) {
            println(b)
            print("Player " + (if (b.iteration % 2 == 0) '0' else 'X') + ": ")
            b.play()
        }
    }
}
