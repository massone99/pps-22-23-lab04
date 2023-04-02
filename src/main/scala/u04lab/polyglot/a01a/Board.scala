package u04lab.polyglot.a01a

import u04lab.code.{Option, *}
import u04lab.code.List.{cons, contains, length}
import u04lab.polyglot.a01a.Logics.Result

import scala.annotation.tailrec
import scala.util.Random

case class Pair(row: Int, col: Int)

trait Board:
	def size: Int

	def numMines: Int

	var cells: List[Pair] =
		var result: List[Pair] = List.Nil()
		for
			row <- 0 until size
			col <- 0 until size
		do
			result = cons(Pair(col, row), result)
		List.reverse(result)

	def minesPositions: List[Pair]

	def hit(row: Int, col: Int): Result

	def isWin: Boolean = length(minesPositions) == length(cells)

object Board:

	import Option.*
	import List.*
	import Stream.*

	def apply(size: Int, numMines: Int): Board =
		BoardImpl(size, numMines)

	private class BoardImpl(override val size: Int, override val numMines: Int) extends Board:

		var minesPositions: List[Pair] =
			println("Number of cells " + length(cells))
			println(cells)
			var mines: List[Pair] = List.Nil()
			var placedMines = 0
			while placedMines < numMines do
				val randomPair: Pair = Pair(Random.nextInt(size), Random.nextInt(size))
				if !contains(mines, randomPair) then
					mines = List.cons(randomPair, mines)
					placedMines = placedMines + 1
			println(mines)
			mines

		/**
		 * Hits a cell and returns true if it contains a mine, false otherwise
		 *
		 * @param row row of the cell
		 * @param col column of the cell
		 * @return true if the cell contains a mine, false otherwise
		 */
		def hit(row: Int, col: Int): Result = {
			val pair = Pair(row, col)
			if contains(minesPositions, pair) then
				Result.LOST
			else
				cells = List.filter(cells)(p => p != pair)
				if isWin then
					Result.WON
				else
					minesPositions = List.filter(minesPositions)(p => p != pair);
					Result.HIT
		}