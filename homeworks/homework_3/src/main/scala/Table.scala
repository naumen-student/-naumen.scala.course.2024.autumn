import java.{util => ju}

case class Table(width: Int, length: Int) {
    private val cells: collection.mutable.MutableList[Cell] = collection.mutable.MutableList.fill(width * length)(EmptyCell())

    def getCell(ix: Int, iy: Int): Option[Cell] =
        if (isCorrect(ix, iy)) {
            Option(cells(ix + iy * width))
        } else {
            None
        }

    def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
        if (isCorrect(ix, iy)) {
            cells(ix + iy * width) = cell
        }
    }

    private def isCorrect(ix: Int, iy: Int): Boolean = !(ix < 0 || ix >= length || iy < 0 || iy >= width)
}