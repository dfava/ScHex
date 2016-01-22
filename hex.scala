import sys.exit

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

    def cellsToLines() = {
        for { rowIdx <- -1 to cells.length } yield {
            if (rowIdx == -1 || rowIdx == cells.length) "  " + (" " * (rowIdx-1)) + ("X " * cells.length)
            else (" " * rowIdx) + "0 " + cells(rowIdx).mkString(" ") + " 0" }
    }

    override def toString() = {
        val board = cellsToLines
        val decoratedBoard = for {
                i <- 0 until board.length
                val spaces = size.toString.length - (i).toString.length + 1
            } yield {
                if (i == 0 || i == cells.length+1) (" " * (spaces+1)) + board(i)
                else (i).toString + (" " * spaces) + board(i)
            }
        decoratedBoard.mkString("\n")
    }
}

object hex {
    def main(args : Array[String]) {
        val size = 10
        val b = Board(size)
        for (i <- 0 to 10) { // TODO: Loop until either one player won or the board is full
            println(b)
            print("Player " + (if (b.iteration % 2 == 0) '0' else 'X') + ": ")
            b.play()
        }
    }
}
