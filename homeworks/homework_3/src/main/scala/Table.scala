class Table(val width: Int, val height: Int) {

  private val cells = Array.fill[Cell](width * height)(new EmptyCell)

  private def isWithinBoundaries(ix: Int, iy: Int): Boolean = ix >= 0 && iy >= 0 && ix < width && iy < height

  private def getCellIndex(ix: Int, iy: Int): Int = ix + iy * width

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (isWithinBoundaries(ix, iy)) Some(cells(getCellIndex(ix, iy))) else None
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (isWithinBoundaries(ix, iy)) cells(getCellIndex(ix, iy)) = cell
  }
}