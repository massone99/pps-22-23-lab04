import org.junit.Assert.*
import org.junit.Test
import u04lab.code.*
import u04lab.polyglot.a01a.*
import u04lab.polyglot.a01a.Logics.Result
class BoardTest {
	@Test def testBoard(): Unit =
		val board = Board(4, 2)
		val testMine: Pair =
			val minesPositions: List[Pair] = board.minesPositions
			List.take(minesPositions, 1) match
				case List.Cons(Pair(row, col), _) => Pair(row, col)
				case _ => throw new Exception("No mines found")
		assertEquals(Result.LOST, board.hit(testMine.row, testMine.col))
}
