/** Hex board game
* An implementation of the hex board game in Scala
* See https://en.wikipedia.org/wiki/Hex_(board_game) for details on the game
*
* RUN
*****
*
* scalac hex.scala
* scala hex
*
* Optional argument -s SIZE can be used to create boards of different sizes.
* Defaults to 14 by 14.
*
* PLAY
******
*
* Type :q to quit
* Type two integers separated by space in order to record a play
*
* TESTING
*********
* Tested the game with two StdInPlayers by running through multiple rounds.
*
* IMPLEMENTATION
****************
* HexBoard is a trait that keeps track of cell states, the list of neighbors of
* each cell, and knows how to turn itself into a string
*
* The HexGame extends HexBoard.  It also knows about the hex game itself by:
*   - Using the HexBoard trait to keep track of cell states,
*   - Knowing which player is supposed to play next by incrementing a counter
*     called iteration,
*   - Calling play() on a Player
*   - Determining whether a player has won
*
* The HexGame takes two Player instances in its constructor.
* Player is an abstract class that defines a play() method.  The play() method
* takes an Int tuple (row, column) with the last move taken by the adversary and
* must return a valid (row, column) tuple representing a new play.
*
* Each Player implementation is responsible for keeping track of the board state.
*
* I provide one concrete implementation of Player called the StdInPlayer,
* which reads a move from standard input.
*
* I started working on an AiPlayer that extends Player, but haven't gotten to
* a place where it is usable yet.  My idea was to keep track of what I call 
* "connected components," which are all the cells that belong to either a 
* player or its opponent that are adjacent to each other.  The AI would use this
* information to find the shortest path between two connected components.
* Since the shortest path calculation is expensive, it would only be done up 
* to a max depth.
* 
* A player would play pseudo randomly until one of two things happened:
*   1) It determine that its opponent could win the game in defendDepth plays
*   2) It determine that some of its connected components are in attackDepth 
*      distance from each other.
*
* If one of these conditions were satisfied, the plays would be targeted at 
* blocking the opponent from winning (in the case of 1), or connecting its own
* components (in the case of 2).
*
* In order to increase the chances of forming connected components, the pseudo 
* random play would favor parts of the board over others.
*
* @author Daniel Fava
* http://github.com/dfava/ScHex.git
*/
import sys.exit
import scala.math._
import scala.collection.mutable.Map

abstract class BoardState;
case class Win(player : Char) extends BoardState;
case class Play(player : Char) extends BoardState;

trait HexBoard {
    val size : Int
    var cells = Array.fill[Char](size, size)('.')
    val neighbors = for {row <- 0 until size} yield // Compute and store indexes of all neighbors
            for {col <- 0 until size} yield createNeighborList(row, col)

    def createNeighborList(cellRow : Int, cellCol : Int) : List[(Int, Int)] = {
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

case class HexGame(size : Int, players : (Player, Player)) extends HexBoard {
    private var iteration = 0 // Incremented every time a player makes a valid move
    private var plays = List[(Int, Int)]()

    def play() {
        val lastPlay = if (plays.isEmpty) None else Some(plays.head)
        val coord = if (iteration % 2 == 0) players._1.play(lastPlay) else players._2.play(lastPlay)
        assert(cells(coord._1)(coord._2) == '.')
        cells(coord._1)(coord._2) = if (iteration % 2 == 0) '0' else 'X'
        plays = coord :: plays
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

    def boardState = {
        val justPlayed = if (iteration % 2 == 0) 'X' else '0'
        val playsNext = if (iteration % 2 == 0) '0' else 'X'
        if (isWin) { Win(justPlayed) } else { Play(playsNext) }
    }
}

abstract class Player { def play(lastPlay : Option[(Int, Int)]) : (Int, Int) }

case class StdInPlayer(size : Int) extends Player with HexBoard { // A player that reads from standard in
    def play(lastPlay : Option[(Int, Int)]) = {
        lastPlay match { // Update board with the last play
            case Some((row, col)) => cells(row)(col) = 'T' // Taken
            case None =>
        }
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
        cells(coord(0)-1)(coord(1)-1) = 'T' // Update the board with the new play
        (coord(0)-1, coord(1)-1)
    }
}

case class AiPlayer(size : Int) extends Player with HexBoard { // An AI player
    // Preventing vars/vals in the outer scope from getting squashed by ConnComp
    var cells_ = cells
    val neighbors_ = neighbors
    case class ConnComp(cells : Set[(Int, Int)]) { // Connected Components
        def this() = this(Set[(Int,Int)]())
        def this(cell : (Int,Int)) = this(Set(cell))
        val neighbors : Set[(Int,Int)] = (for (c <- cells) yield neighbors_(c._1)(c._2)).flatten
        def contains(cell : (Int, Int)) : Boolean = { cells.contains(cell) }
        def isNeighbor(cell : (Int, Int)) : Boolean = { neighbors.contains(cell) }
        def +(cell : (Int, Int)) : ConnComp = { assert(isNeighbor(cell)); ConnComp(cells + cell) }
        def ++(c : ConnComp) : ConnComp = { ConnComp(cells ++ c.cells) }
    }
    val defendDepth = 3
    val attackDepth = 3
    private var playerConnComps = Map( 'O' -> List[ConnComp](), 'I' -> List[ConnComp]())

    def shortestDistance(c : ConnComp) {
        // TODO: Work in progress...
        val freeNeighbors = for { 
                n <- c.neighbors
                if cells(n._1)(n._2) == '.'
            } yield n
        val newC = c ++ new ConnComp(freeNeighbors)
        println(newC)
    }

    def addCell(player: Char, cell : (Int, Int)) {
        cells(cell._1)(cell._2) = player // Update board

        // Cell is a neighbor of one of more ConnComps
        // in which case we merge them and add the cell to the merger
        val toMerge = playerConnComps(player).filter((c : ConnComp) => c.isNeighbor(cell))
        if (toMerge.length > 0) {
            val toKeep = playerConnComps(player).filter((c : ConnComp) => !c.isNeighbor(cell))
            playerConnComps(player) = (toMerge.fold(new ConnComp(cell)){(a,i) => a ++ i}) :: toKeep
            // Debug prints
            print("toKeep: "); println(toKeep)
            print("toMerge: "); println(toMerge.fold(new ConnComp(cell)) {(a,i) => a ++ i})
        }

        // Or Cell is not a neighbor of a component
        // Make a new ConnComp containing once cell
        if (toMerge.length == 0) {
            playerConnComps(player) = new ConnComp(cell) :: playerConnComps(player)
        }

        println(playerConnComps(player))
    }

    // Determines whether a player has a sequence of d plays that can end the game
    def analyze(player : Char, depth : Int) = {
        // TODO
    }

    def play(lastPlay : Option[(Int,Int)]) = {
        lastPlay match { // Update board with the Opponent's last play
            case Some(cell) => addCell('O', cell)
            case None =>
        }
        // Figure out whether we need to defend (block)
        // TODO
        (0,0)
    }
}

object hex {
    val default = Map('size -> "14")
    val usage = s"""usage: hex [-h] [-s SIZE]

Hex board game

optional arguments:
  -h, -help             show this help message and exit
  -s SIZE               set the board size (default: $default('size))"""

    def gameRound(b : HexGame) {
        // Don't want to put this inside Board in order to keep the board generic
        // This function deals with printing to stdout.  
        // It is up to the board API caller wether, for example, the board state 
        // ought to be printed to stdout 
        b.boardState match {
            case Win(player) => println(s"Player $player won!")
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
        val size = (default ++ options)('size).toInt
        gameRound(HexGame(size, (new StdInPlayer(size), new StdInPlayer(size))))
    }
}
