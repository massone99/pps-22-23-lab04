package u04lab.polyglot.a01a

import Logics.*
import u04lab.polyglot.a01a.*

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val numMines: Int) extends Logics:
	val board: Board = Board(size, numMines)

	def hit(row: Int, col: Int): Result =
		board.hit(row, col)