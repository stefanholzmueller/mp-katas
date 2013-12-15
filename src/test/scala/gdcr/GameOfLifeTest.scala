package gdcr

import org.junit.Test
import scala.collection.immutable.HashSet

class GameOfLifeTest extends GameOfLife {

	def nthGeneration(n: Int, initial: Set[Cell]): Set[Cell] = {
		if (n == 0)
			initial
		else {
			nthGeneration(n - 1, nextGeneration(initial))
		}
	}

	@Test
	def testEmptyColonyStaysEmpty {
		val emptySet = Set[Cell]()

		val next = nextGeneration(emptySet)

		assert(next == emptySet)
	}

	@Test
	def testSingleLivingCellDies {
		val singleCell = Set[Cell](Array(0, 0))

		val next = nextGeneration(singleCell)

		assert(next == Set[Cell]())
	}

	@Test
	def testLivingCellWithTwoLivingNeighborsSurvives {
		val initial = Set[Cell](Array(0, 0), Array(1, 0), Array(2, 0))

		val next = nextGeneration(initial)

		assert(next.contains(Array(1, 0)), next)
	}

	@Test
	def testLivingCellWithFourLivingNeighborsDies {
		val initial = Set[Cell](Array(0, 0), Array(1, 0), Array(2, 0), Array(1, -1), Array(1, 1))

		val next = nextGeneration(initial)

		assert(!next.contains(Array(1, 0)), next)
	}

	@Test
	def testBlinker {
		val initial = Set[Cell](Array(-1, 0), Array(0, 0), Array(1, 0))
		val alternative = Set[Cell](Array(0, -1), Array(0, 0), Array(0, 1))

		val gen1 = nextGeneration(initial)
		val gen2 = nthGeneration(2, initial)

		assert(gen1 == alternative, gen1)
		assert(gen2 == initial, gen2)
	}

	@Test
	def testGlider {
		val initial = Set[Cell](Array(1, 3), Array(2, 2), Array(3, 2), Array(3, 3), Array(3, 4))

		val gen4 = nthGeneration(4, initial)

		assert(gen4 == Set[Cell](Array(2, 2), Array(3, 1), Array(4, 1), Array(4, 2), Array(4, 3)), gen4)
	}

	@Test
	def testCube {
		val initial = findNeighbors(List(0, 0, 0)).toSet

		val next = nextGeneration(initial)

		assert(next.size == 12, next)
	}

	@Test
	def testNeighborsOf2DList {
		val origin = List(0, 0)

		val neighbors = findNeighbors(origin)

		assert(neighbors.size == 8, neighbors.size)
		assert(neighbors.contains(List(1, 1)), neighbors)
		assert(!neighbors.contains(List(0, 0)), neighbors)
	}

	@Test
	def testNeighborsOf3DArray {
		val origin = Array(1, 2, -3)

		val neighbors = findNeighbors(origin)

		assert(neighbors.size == 26, neighbors.size)
		assert(neighbors.contains(List(1, 2, -2)), neighbors)
		assert(!neighbors.contains(List(1, 2, -3)), neighbors)
	}

}