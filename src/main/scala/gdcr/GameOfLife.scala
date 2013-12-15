package gdcr

import scalaz._
import Scalaz._

trait GameOfLife {

	type Cell = Seq[Int]

	val UNDERPOPULATION_MAX = 1
	val OVERCROWDING_MIN = 4
	val REPRODUCTION_EXACTLY = 3

	def findNeighbors(cell: Cell): List[Cell] = {
		val offsets = List(1, 0, -1)
		val combinations = Stream.continually(offsets).take(cell.size).toList
		val crossProduct = combinations.toList.sequence // scalaz!
		val neighborOffsets = crossProduct.filterNot(_.forall(_ == 0))
		neighborOffsets.map(_.zip(cell).map { case (a, b) => a + b })
	}

	def nextGeneration(livingCells: Set[Cell]): Set[Cell] = {
		def survive(cell: Cell): Boolean = {
			val neighbors = findNeighbors(cell)
			val livingNeighbors = neighbors.toSet.intersect(livingCells).size
			UNDERPOPULATION_MAX < livingNeighbors && livingNeighbors < OVERCROWDING_MIN
		}

		val survivedCells = livingCells.filter(survive(_))

		val possiblyBornCells = livingCells.map(findNeighbors(_)).flatten
		val bornCells = possiblyBornCells.filter(p => findNeighbors(p).toSet.intersect(livingCells).size == REPRODUCTION_EXACTLY)

		survivedCells.union(bornCells)
	}

}