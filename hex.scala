/** Hex board game
* An implementation of the hex board game in Scala
* See https://en.wikipedia.org/wiki/Hex_(board_game) for details on the game
*
* @author Daniel Fava
* http://github.com/dfava/ScHex.git
*/
import sys.exit
import scala.math._

abstract class BoardState;
case class Win(player : Char) extends BoardState;
case class Tie extends BoardState;
case class Play(player : Char) extends BoardState;

case class Board(size : Int, players : (Player, Player)) {
    private var cells = Array.fill[Char](size, size)('.')
    private var iteration = 0 // Incremented every time a player makes a valid move

    private val neighbors = for {row <- 0 to cells.length-1} yield // Compute and store indexes of all neighbors
            for {col <- 0 to cells.length-1} yield createNeighborList(row, col)

    private def createNeighborList(cellRow : Int, cellCol : Int) : List[(Int, Int)] = {
        (for {
            offsetRow <- -1 to 1
            offsetCol <- -1 to 1
            if !(offsetRow == 0 && offsetCol == 0)
            if !(cellRow % 2 == 0 && List((-1,1), (1,1)).contains((offsetRow, offsetCol)))
            if !(cellRow % 2 == 1 && List((-1,-1), (1,-1)).contains((offsetRow, offsetCol)))
            row = cellRow + offsetRow
            col = cellCol + offsetCol
            if row >= 0 && col >= 0 && row < size && col < size
        } yield (row,col)).toList
    }

    def play() {
        // TODO: Must pass a copy of cells
        val coord = if (iteration % 2 == 0) players._1.play(cells) else players._2.play(cells)
        cells(coord._1)(coord._2) = if (iteration % 2 == 0) '0' else 'X'
        iteration += 1
    }

    def isWin : Boolean = { // Detect whether a player has won, or whether the board is full
        val player = if (iteration % 2 == 0) 'X' else '0' // Inverted on purpose. We check at the end of a play
        val toExplore = player match {
            case 'X' => for { i <- 0 until size if (cells(0)(i) == player) } yield (0,i)
            case '0' => for { i <- 0 until size if (cells(i)(0) == player) } yield (i,0)
        }
        isWinRec(toExplore.toList, List[(Int,Int)]())
    }

    private def isWinRec(toExplore : List[(Int, Int)], explored : List[(Int, Int)]) : Boolean = {
        val player = if (iteration % 2 == 0) 'X' else '0' // Inverted on purpose. We check at the end of a play
        //println("Checking win for player " + player)
        toExplore match {
            case head :: tail if player == 'X' && head._1 == size-1 => true // X arrived on the other side
            case head :: tail if player == '0' && head._2 == size-1 => true // 0 arrived on the other side
            case head :: tail => {
                val neighborsOfX = for { 
                        n <- neighbors(head._1)(head._2)
                        if (cells(n._1)(n._2) == player) 
                    } yield n
                isWinRec((neighborsOfX diff explored) ::: tail, head :: explored)
            }
            case Nil => false
        }
    }

    // TODO: Is a tie even possible?
    def isTie : Boolean = { (iteration >= size * size) } // Detect ties

    def boardState = {
        val justPlayed = if (iteration % 2 == 0) 'X' else '0'
        val playsNext = if (iteration % 2 == 0) '0' else 'X'
        if (isWin) { Win(justPlayed) } else if (isTie) { Tie } else { Play(playsNext) }
    }

    private def cellsToLines() = { // Helper function to toString
        // Transforms cells into an array of strings, each string being a board row
        for { rowIdx <- -1 to size} yield {
            if (rowIdx == -1 || rowIdx == size) "  " + (" " * (rowIdx-1)) + ("X " * size)
            else (" " * rowIdx) + "0 " + cells(rowIdx).mkString(" ") + " 0" }
    }

    private def colNumbers = { // Helper function to toString
        // Return a vector representing the indices of every column
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

abstract class Player { def play(board: Array[Array[Char]]) : (Int, Int) }

class StdInPlayer extends Player { // A player that reads from standard in
    def play(board: Array[Array[Char]]) : (Int, Int) = {
        val size = board.length
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
        while (board(coord(0)-1)(coord(1)-1) != '.') {
            println("Try again.  That cell is already taken.")
            coord = readStdIn()
        }
        (coord(0)-1, coord(1)-1)
    }
}

class AiPlayer extends Player { // An AI player
    def play(board: Array[Array[Char]]) : (Int, Int) = {
        (0,0) // TODO
    }
}

object hex {
    val default = Map('size -> "10")
    val usage = s"""usage: hex [-h] [-s SIZE]

Hex board game

optional arguments:
  -h, -help             show this help message and exit
  -s SIZE               set the board size (default: $default('size))"""

    def gameRound(b : Board) {
        // Don't want to put this inside Board in order to keep the board generic
        // This function deals with printing to stdout.  
        // It is up to the board API caller wether, for example, the board state 
        // ought to be printed to stdout 
        b.boardState match {
            case Win(player) => println(s"Player $player won!")
            case Tie => println("Its a tie!")
            case Play(player) => {
                println(b)
                print(s"Player $player: ")
                b.play()
                gameRound(b)
            }
        }
    }

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
        gameRound(Board((default ++ options)('size).toInt, (new StdInPlayer(), new StdInPlayer())))
    }
}
